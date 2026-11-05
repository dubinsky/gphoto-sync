# Clock

## Rank

Each picture has one civil date and time.
The folder day is that civil date.
`ls` prints the source that won.

A sidecar `timestamp` replaces the date and the time.
A sidecar `date` replaces the day and keeps the time from the next source.
When both are set and the days agree, the timestamp's day is the date.
A file where they disagree is reported by path and is not moved.
`repair --days` is the move.
`item set date=<day>` stores that civil day and moves the files there.
Clearing `date` does not move the files.
Any other `item set` leaves the files in their folder.

Otherwise one embedded capture time is used, from one file: a JPEG-class still, else a HEIC or other image, else a RAW,
else a video.
A file with no parseable capture time does not fall through to the next file.

A filename clock is used only when no usable embedded clock remains, including a missing clock and a sentinel
`DateTimeOriginal` together with a sentinel `CreateDate`.
Full, compact, and short names are civil digits.
A 13-digit name is a UTC instant converted with the archive zone.
One trailing `v`, then one trailing `_1`, is ignored.
The stored stem is unchanged.
When nothing else is known and the picture is in a day folder, the day stays that folder day.
A picture that is not in a day folder, including `.upload`, has no folder-day clock and is not stamped.

## Tags

Inside a video, a missing or sentinel CreationDate falls through to ffprobe.
Thumbnails and `.tif` auxiliaries are read and do not vote.
Inside a still, a usable `DateTimeOriginal` wins over `CreateDate`.
A sentinel `DateTimeOriginal` (January 1 of 1980 or of 2000) does not block `CreateDate`.
If both are sentinels, there is no embedded clock.
The digits are the civil clock.
`OffsetTimeOriginal` is kept when the tag is present and does not change that civil day.
`ModifyDate` and mtime do not vote.
CR3 is read the same way.
CRW stays on dcraw, as a civil clock.

Times more than 60 minutes apart are reported.
The report does not change the winner.
mtime is compared to the second.
`repair --timestamps` writes the civil clock in the archive zone.
The read uses that same conversion.

Publish writes the sidecar `timestamp` as JPEG `DateTimeOriginal` or QuickTime `CreateDate`, with no offset.
Album membership uses the folder day.
A Google item joins on the catalog civil day plus the stem.
A stem found only on the previous or next day is a date mismatch.
It is not a second copy.
