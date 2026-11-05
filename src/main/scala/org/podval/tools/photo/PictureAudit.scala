package org.podval.tools.photo

import java.time.{Instant, LocalDateTime, ZoneOffset}

// The clock and day assessment printed by ls and applied by repair.
object PictureAudit:
  def apply(picture: Picture): Dates.Assessment =
    assessed(
      picture,
      picture.extensions.toSeq.map: extension =>
        Dates.Observed(extension.name, extension.votes, extension.read),
      picture.extensions.map(_.current),
      picture.timestampFromName.map(_._1),
      picture.metadata
    )

  def noAssessedTimestamp(audit: Dates.Assessment): Boolean =
    audit.clock.forall(_.source == Dates.Source.FolderDay)

  // observed is already read, so a misnamed file can supply the bytes without the corrected path.
  private[photo] def assessed(
      picture: Picture,
      observed: Seq[Dates.Observed],
      currents: Iterable[Instant],
      filename: Option[Dates.Clock],
      metadata: Metadata
  ): Dates.Assessment =
    // A timestamp that does not parse still throws, as it does today.
    val timestamp: Option[LocalDateTime] = metadata.timestamp.map(LocalDateTime.parse)
    val usableName: Option[Dates.Clock] = Dates.usable(filename)
    val preferred: Option[String] = Dates.captureDescriptor(picture).map(_.name)
    val embedded: Option[Dates.Clock] = preferred.flatMap: name =>
      observed.find(_.extension == name).flatMap(row => Dates.usable(row.clock))
    val folder: Option[Dates.Clock] = picture.parent.day.map: day =>
      Dates.Clock(day.date.atStartOfDay, None, Dates.Source.FolderDay)
    val ranked: Dates.Ranked = Dates.rank(
      Dates.RankInput(
        timestamp,
        metadata.date,
        embedded,
        usableName,
        folder
      )
    )
    val winner: Set[LocalDateTime] = ranked match
      case Dates.Ranked.Chosen(clock) if clock.source != Dates.Source.FolderDay => Set(clock.civil)
      case _                                                                    => Set.empty
    val voting: Set[LocalDateTime] = observed.iterator
      .filter(_.votes)
      .flatMap(row => Dates.usable(row.clock))
      .map(_.civil)
      .toSet
    val vary: Set[LocalDateTime] =
      timestamp.toSet ++ winner ++ voting ++ usableName.map(_.civil).toSet
    val timestampsVary: Boolean =
      vary.size >= 2 &&
        Dates.minutesApart(
          vary.minBy(_.toEpochSecond(ZoneOffset.UTC)),
          vary.maxBy(_.toEpochSecond(ZoneOffset.UTC))
        ) > Dates.varyMinutes
    Dates.assessment(picture, ranked, timestampsVary, currents)
