unit myutils;

interface

function UnixMillisToDateTime(t: int64): TDateTime;
function DateTimeToUnixMillis(t: TDateTime): int64;

implementation

uses dateutils;

var
    unixTime: TDateTime;

function unixMillisToDateTime(t: int64): TDateTime;
begin
    result := IncHour(IncMilliSecond(unixTime, t), 3);
end;

function DateTimeToUnixMillis(t: TDateTime): int64;
begin
    result := MilliSecondsBetween(unixTime, IncHour(t, -3));
end;

initialization

unixTime := EncodeDateTime(1970, 1, 1, 0, 0, 0, 0);

end.
