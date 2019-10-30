unit UnitMeasurement;

interface

type

    TMeasurement = record
    public
        Temperature: Double;
        Pressure: Double;
        Humidity: Double;
        Places: array [0..49] of Double;
        StoredAt: TDateTime;

        class function Deserialize(var p: Pointer): TMeasurement; static;
        class function DeserializeMeasurements(var p: Pointer): TArray<TMeasurement>;  static;

    end;

    TProductMeasurement = record
    public
        Temperature: Double;
        Pressure: Double;
        Humidity: Double;
        Value: Double;
        StoredAt: TDateTime;

        class function Deserialize(var p: Pointer): TProductMeasurement; static;
        class function DeserializeMeasurements(var p: Pointer): TArray<TProductMeasurement>;  static;

    end;

implementation

uses myutils;

class function TMeasurement.DeserializeMeasurements(var p: Pointer): TArray<TMeasurement>;
var i,measurementsCount:int64;
begin
    measurementsCount := PInt64(p)^;
    Inc(PByte(p), 8);
    SetLength(Result, measurementsCount);
    for i:=0 to measurementsCount-1 do
        Result[i] := TMeasurement.Deserialize(p);
end;

class function TMeasurement.Deserialize(var p: Pointer): TMeasurement;
var
    I: Integer;
begin

    Result.StoredAt := unixMillisToDateTime(Pint64(p)^);
    Inc(PByte(p), 8);

    Result.Temperature := PDouble(p)^;
    Inc(PByte(p), 8);

    Result.Pressure := PDouble(p)^;
    Inc(PByte(p), 8);

    Result.Humidity := PDouble(p)^;
    Inc(PByte(p), 8);

    for I := 0 to 49 do
    begin
        Result.Places[i] := PDouble(p)^;
        Inc(PByte(p), 8);
    end;
end;


class function TProductMeasurement.DeserializeMeasurements(var p: Pointer): TArray<TProductMeasurement>;
var i,measurementsCount:int64;
begin
    measurementsCount := PInt64(p)^;
    Inc(PByte(p), 8);
    SetLength(Result, measurementsCount);
    for i:=0 to measurementsCount-1 do
        Result[i] := TProductMeasurement.Deserialize(p);
end;

class function TProductMeasurement.Deserialize(var p: Pointer): TProductMeasurement;
var
    I: Integer;
begin

    Result.StoredAt := unixMillisToDateTime(Pint64(p)^);
    Inc(PByte(p), 8);

    Result.Temperature := PDouble(p)^;
    Inc(PByte(p), 8);

    Result.Pressure := PDouble(p)^;
    Inc(PByte(p), 8);

    Result.Humidity := PDouble(p)^;
    Inc(PByte(p), 8);

    Result.Value := PDouble(p)^;
        Inc(PByte(p), 8);
end;

end.
