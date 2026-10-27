package org.podval.tools.photo

import java.time.{Instant, LocalDate}

// The timestamp and day assessment printed by ls and applied by repair.
object PictureAudit:
  final case class Result(
    timestampsVary: Boolean,
    timestamp: Option[Instant],
    timestampWrong: Boolean,
    dayShouldBe: Option[Day]
  )

  def apply(picture: Picture): Result =
    // Note: in the pictures saw where metadata timestamp is present and file name also encodes the timestamp:
    // - for '3gp' and 'mp4' metadata timestamp is sometimes later than the file name timestamp
    //   (and a few hours for the timezone offset) - metadata probably records timestamp of the end of recording;
    // - for 'jpg' metadata timestamp is sometimes earlier than the file name timestamp by a second;
    // I pick the earliest ;)
    val timestamps: Set[Option[Instant]] =
      picture.extensions.map(_.fromMetadata) ++ Set(picture.timestampFromName.map(_._1))
    val timestampEarliest: Option[Instant] = Dates.earliest(timestamps)
    val timestampLatest: Option[Instant] = Dates.latest(timestamps)
    val timestampsVary: Boolean = timestampEarliest.isDefined && timestampLatest.isDefined &&
      Dates.distanceInMinutes(timestampEarliest.get, timestampLatest.get) > 60

    val timestamp: Option[Instant] = Dates.FromMetadata.get(picture).orElse(timestampEarliest)

    val timestampWrong: Boolean = timestamp.fold(false)(timestamp =>
      picture.extensions.exists(extension => !Dates.equal(timestamp, extension.current))
    )

    val dayShouldBe: Option[Day] = timestamp.flatMap: timestamp =>
      val date: LocalDate = Dates.toZonedDateTime(timestamp).toLocalDate
      val year: Int = date.getYear
      val month: Int = date.getMonthValue
      val day: Int = date.getDayOfMonth

      // some photos were taken with no time set on the camera...
      val isDefaultDate: Boolean = month == 1 && day == 1 && (year == 1980 || year == 2000)
      val isCorrectDay: Boolean = picture.parent.day.exists(_.date.isEqual(date))
      Option.when(!isDefaultDate && !isCorrectDay)(picture.root.year(year).month(month).day(day))

    Result(timestampsVary, timestamp, timestampWrong, dayShouldBe)
