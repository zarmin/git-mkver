package net.cardnell.mkver

import java.time.LocalDate

import zio.blocking.Blocking
import zio.console.{Console, putStrLnErr}
import zio.{IO, RIO, Task, UIO, ZIO}

object MkVer {
  case class CommitInfo(shortHash: String, fullHash: String, commitsBeforeHead: Int, tags: List[Version])

  case class LastVersion(commitHash: String, commitsBeforeHead: Int, version: Version)

  val commitInfoLineMatch = "^([0-9a-f]{5,40}) ([0-9a-f]{5,40}) *(\\((.*)\\))?$".r

  def tagNames(names: String): List[String] =
    Option(names).getOrElse("").split(",").toList
      .map(_.trim)
      .filter(_.startsWith("tag: "))
      .map(_.replace("tag: ", ""))

  def getCommitInfos(prefix: String): RIO[Git with Blocking, List[CommitInfo]] = {
    Git.commitInfoLog().map { log =>
      log.linesIterator.zipWithIndex.flatMap {
        case (line, i) => {
          line match {
            case commitInfoLineMatch(shortHash, longHash, _, names) => {
              val versions = tagNames(names).flatMap(Version.parseTag(_, prefix))
              Some(CommitInfo(shortHash, longHash, i, versions))
            }
            case _ => None
          }
        }
      }.toList
    }
  }

  def getLastVersion(commitInfos: List[CommitInfo]): Option[LastVersion] = {
    commitInfos.find(_.tags.nonEmpty).map(ci => LastVersion(ci.fullHash, ci.commitsBeforeHead, ci.tags.max))
  }

  def formatVersion(config: RunConfig, versionData: VersionData, formatAsTag: Boolean, preRelease: Boolean): Task[String] = {
    Task.effect {
      val formatter = Formatter(versionData, config, preRelease)
      if (formatAsTag) {
        formatter.format("{Tag}")
      } else {
        formatter.format("{Next}")
      }
    }
  }

  def getNextVersion(config: RunConfig, currentBranch: String, preRelease: Boolean, verbose: Boolean = false): RIO[Console with Git with Blocking, VersionData] = {
    for {
      commitInfos <- getCommitInfos(config.tagPrefix)
      lastVersionOpt = getLastVersion(commitInfos)
      bumps <- getVersionBumps(currentBranch, lastVersionOpt, config.commitMessageActions, config.whenNoValidCommitMessages)
      nextVersion = lastVersionOpt.map(_.version.getNextVersion(bumps, preRelease)).getOrElse(NextVersion(0, 1, 0, if (preRelease) Some(1) else None))
      versionData = VersionData(
        major = nextVersion.major,
        minor = nextVersion.minor,
        patch = nextVersion.patch,
        preReleaseNumber = nextVersion.preReleaseNumber,
        commitCount = lastVersionOpt.map(_.commitsBeforeHead).getOrElse(commitInfos.length),
        branch = currentBranch,
        commitHashShort = commitInfos.headOption.map(_.shortHash).getOrElse(""),
        commitHashFull = commitInfos.headOption.map(_.fullHash).getOrElse(""),
        date = LocalDate.now()
      )
      _ <- explainNextVersion(config, currentBranch, preRelease, commitInfos, lastVersionOpt, bumps, nextVersion, versionData).when(verbose)
    } yield versionData
  }

  def explainNextVersion(config: RunConfig,
                         currentBranch: String,
                         preRelease: Boolean,
                         commitInfos: List[CommitInfo],
                         lastVersionOpt: Option[LastVersion],
                         bumps: VersionBumps,
                         nextVersion: NextVersion,
                         versionData: VersionData): RIO[Console with Git with Blocking, Unit] = {
    for {
      infoLog <- Git.commitInfoLog()
      allTags <- Git.allTags()
      fullLog <- lastVersionOpt match {
        case None => Git.fullLog(None)
        case Some(LastVersion(_, 0, _)) => UIO.succeed("")
        case Some(lv) => Git.fullLog(Some(lv.commitHash))
      }
      nextTag <- formatVersion(config, versionData, formatAsTag = true, preRelease = preRelease)
      lines = explainLines(config, currentBranch, preRelease, commitInfos, lastVersionOpt, bumps, nextVersion, nextTag, infoLog, allTags, fullLog)
      _ <- ZIO.foreach_(lines)(line => putStrLnErr(s"[verbose] $line"))
    } yield ()
  }

  def explainLines(config: RunConfig,
                   currentBranch: String,
                   preRelease: Boolean,
                   commitInfos: List[CommitInfo],
                   lastVersionOpt: Option[LastVersion],
                   bumps: VersionBumps,
                   nextVersion: NextVersion,
                   nextTag: String,
                   infoLog: String,
                   allTags: String,
                   fullLog: String): List[String] = {
    val prefix = config.tagPrefix
    val ordering = Version.versionOrdering
    def show(v: Version): String =
      prefix + s"${v.major}.${v.minor}.${v.patch}" + v.preRelease.map("-" + _).getOrElse("") + v.buildMetaData.map("+" + _).getOrElse("")

    val reachableTags = infoLog.linesIterator.toList.collect {
      case commitInfoLineMatch(shortHash, _, _, names) if tagNames(names).nonEmpty => (shortHash, tagNames(names))
    }
    val reachableTagNames = reachableTags.flatMap(_._2).toSet

    val header = List(
      s"branch: '$currentBranch'",
      s"tagPrefix: '$prefix', whenNoValidCommitMessages: ${config.whenNoValidCommitMessages}, pre-release: $preRelease",
      s"scanned ${commitInfos.length} commits reachable from HEAD"
    )

    val tagLines = if (reachableTags.isEmpty) {
      List("no tags found on commits reachable from HEAD (shallow clone or tags not fetched?)")
    } else {
      "tags on commits reachable from HEAD (git log order):" :: reachableTags.flatMap { case (hash, names) =>
        names.map { name =>
          val status = Version.parseTag(name, prefix)
            .map(_ => "valid")
            .getOrElse(s"ignored, does not match '$prefix<major>.<minor>.<patch>[-<pre>][+<meta>]'")
          s"  $hash $name -> $status"
        }
      }
    }

    val lastVersionLines = lastVersionOpt match {
      case None =>
        List("no valid version tag reachable from HEAD -> next version starts at 0.1.0, increments are ignored")
      case Some(lv) =>
        val highest = commitInfos.flatMap(_.tags).max(ordering)
        List(
          Some(s"last version: ${show(lv.version)} on commit ${lv.commitHash.take(7)}, ${lv.commitsBeforeHead} commits before HEAD (first tagged commit in git log order, highest tag on that commit)"),
          lv.version.preRelease.map(pr => s"last version is a pre-release '$pr', parsed pre-release number: ${lv.version.preReleaseNumber().getOrElse("none")}"),
          if (ordering.gt(highest, lv.version)) Some(s"WARNING: higher version ${show(highest)} is reachable from HEAD but is not on the nearest tagged commit, so it is not used") else None
        ).flatten
    }

    val commitLines = lastVersionOpt match {
      case Some(LastVersion(_, 0, _)) =>
        List("HEAD is tagged with the last version -> no increments")
      case _ =>
        val range = lastVersionOpt.map(lv => s"${lv.commitHash.take(7)}..HEAD").getOrElse("all commits")
        val overrideRegex = "    next-version: *(\\d+\\.\\d+\\.\\d+)".r
        val commits = fullLog.linesIterator.foldLeft(List.empty[(String, List[String])]) { (acc, line) =>
          if (line.startsWith("commit")) {
            (line.split(" ").lift(1).getOrElse("").take(7), Nil) :: acc
          } else if (line.startsWith("    ") && acc.nonEmpty) {
            (acc.head._1, acc.head._2 :+ line) :: acc.tail
          } else {
            acc
          }
        }.reverse
        s"commits examined for increments ($range): ${commits.length}" :: commits.flatMap { case (hash, messageLines) =>
          val subject = messageLines.map(_.trim).find(_.nonEmpty).getOrElse("")
          val matches = messageLines.flatMap { line =>
            val overrideMatch = line match {
              case overrideRegex(v) => Some(s"      '${line.trim}' -> override next version to $v")
              case _ => None
            }
            overrideMatch.toList ++ matchingAction(line, config.commitMessageActions)
              .map(cma => s"      '${line.trim}' matches '${cma.pattern}' -> ${cma.action}")
          }
          s"  $hash '$subject'" :: (if (matches.isEmpty) List("      no matching line") else matches)
        }
    }

    val logBumps = calcBumps(fullLog.linesIterator.toList, config.commitMessageActions, VersionBumps())
    val bumpLines = List(
      if (lastVersionOpt.forall(_.commitsBeforeHead != 0) && logBumps.noValidCommitMessages())
        Some(s"no commit message requested an increment -> whenNoValidCommitMessages ${config.whenNoValidCommitMessages} applied")
      else None,
      Some(s"increments: major=${bumps.major}, minor=${bumps.minor}, patch=${bumps.patch}"),
      bumps.commitOverride.map(v => s"commit message override 'next-version: ...' -> ${v.major}.${v.minor}.${v.patch} (takes precedence over increments)"),
      bumps.branchNameOverride.map(v => s"branch name '$currentBranch' forces version ${v.major}.${v.minor}.${v.patch} (takes precedence over increments)")
    ).flatten

    val preReleaseLines = lastVersionOpt.map { lv =>
      (lv.version.preRelease, preRelease) match {
        case (Some(_), false) => "last version is a pre-release, next is a release -> release of the same version, increments ignored (overrides still apply)"
        case (Some(_), true) => "last and next versions are pre-releases -> same version, pre-release number + 1 (unless overridden to a different version)"
        case (None, true) => "last version is a release, next is a pre-release -> apply increments, pre-release number starts at 1"
        case (None, false) => "last and next versions are releases -> apply increments"
      }
    }.toList

    val existingTags = allTags.linesIterator.map(_.trim).filter(_.nonEmpty).toList
      .flatMap(t => Version.parseTag(t, prefix).map(v => (t, v)))
    val collisions = existingTags.filter { case (_, v) =>
      v.equalsVersionCore(Version(nextVersion.major, nextVersion.minor, nextVersion.patch)) &&
        (v.preRelease.isEmpty == nextVersion.preReleaseNumber.isEmpty) &&
        (nextVersion.preReleaseNumber.isEmpty || v.preReleaseNumber() == nextVersion.preReleaseNumber)
    }
    val collisionLines = collisions.map { case (t, _) =>
      val where = if (reachableTagNames.contains(t)) "reachable from HEAD" else "NOT reachable from HEAD (another branch or unmerged commit), so it was not considered"
      s"WARNING: tag '$t' with the same version already exists, $where"
    }
    val highestOverallLines = existingTags.filterNot(t => reachableTagNames.contains(t._1)).map(_._2).reduceOption(ordering.max).filter { highest =>
      lastVersionOpt.forall(lv => ordering.gt(highest, lv.version))
    }.map(h => s"note: version ${show(h)} is tagged in the repository, higher than the last version, but it is not reachable from HEAD").toList

    header ++ tagLines ++ lastVersionLines ++ commitLines ++ bumpLines ++ preReleaseLines ++
      List(s"next version: $nextTag") ++ collisionLines ++ highestOverallLines
  }

  def getVersionBumps(currentBranch: String,
                      lastVersion: Option[LastVersion],
                      commitMessageActions: List[CommitMessageAction],
                      whenNoValidCommitMessages: IncrementAction): RIO[Git with Blocking, VersionBumps] = {
    def logToBumps(log: String): IO[MkVerException, VersionBumps] = {
      val logBumps: VersionBumps = calcBumps(log.linesIterator.toList, commitMessageActions, VersionBumps())
      if (logBumps.noValidCommitMessages()) {
        getFallbackVersionBumps(whenNoValidCommitMessages, logBumps)
      } else {
        RIO.succeed(logBumps)
      }
    }

    val bumps = lastVersion match {
      case None => Git.fullLog(None).flatMap(logToBumps) // No previous version
      case Some(LastVersion(_, 0, _)) => RIO.succeed(VersionBumps.none) // This commit is a version
      case Some(lv) => Git.fullLog(Some(lv.commitHash)).flatMap(logToBumps)
    }

    val releaseBranch = "^(hotfix|rel|release)[-/](\\d+\\.\\d+\\.\\d+)$".r
    val branchNameOverride = currentBranch match {
      case releaseBranch(_, v) => Version.parseTag(v, "")
      case _ => None
    }

    bumps.map(_.withBranchNameOverride(branchNameOverride))
  }

  def getFallbackVersionBumps(whenNoValidCommitMessages: IncrementAction, logBumps: VersionBumps): IO[MkVerException, VersionBumps] = {
    whenNoValidCommitMessages match {
      case IncrementAction.Fail => IO.fail(MkVerException("No valid commit messages found describing version increment"))
      case IncrementAction.NoIncrement => IO.succeed(logBumps)
      case other => IO.succeed(logBumps.bump(other))
    }
  }

  def calcBumps(lines: List[String], commitMessageActions: List[CommitMessageAction], bumps: VersionBumps): VersionBumps = {
    val overrideRegex = "    next-version: *(\\d+\\.\\d+\\.\\d+)".r
    if (lines.isEmpty) {
      bumps
    } else {
      val line = lines.head
      if (line.startsWith("commit")) {
        calcBumps(lines.tail, commitMessageActions, bumps.bumpCommits())
      } else if (line.startsWith("    ")) {
        // check for override text
        val newBumps = line match {
          case overrideRegex(v) => Version.parseTag(v, "").map(bumps.withCommitOverride).getOrElse(bumps)
          case _ => bumps
        }
        // check for bump messages
        val newBumps2 = matchingAction(line, commitMessageActions).map(cma => newBumps.bump(cma.action)).getOrElse(newBumps)
        calcBumps(lines.tail, commitMessageActions, newBumps2)
      } else {
        calcBumps(lines.tail, commitMessageActions, bumps)
      }
    }
  }

  def matchingAction(line: String, commitMessageActions: List[CommitMessageAction]): Option[CommitMessageAction] =
    commitMessageActions.find(cma => cma.pattern.r.findFirstIn(line).nonEmpty)
}
