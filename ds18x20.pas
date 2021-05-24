unit DS18X20;

{$mode objfpc}{$H+}
{$DEFINE DEBUG}
interface

uses OneWire, SysUtils;

const
  CONVERT_T        = $44; // Конвертировать температуру
  WRITE_DATA       = $4E; // Записать данные
  READ_DATA        = $BE; // Прочитать данные
  WRITE_EEPROM     = $48; // Копировать настройки в ЕEPROM
  READ_EEPROM      = $B8; // Копировать настройки из ЕЕPROM

  fMEASURE         = ' C';

type

  {TDS18S20}
  PDS18X20   = ^TDS18X20;

  TDS18X20   = record
    TEMP     : SmallInt ;         // 2
    TEMP_MAX : ShortInt ;         // 1
    TEMP_MIN : ShortInt ;         // 1
    CONF     : Byte     ;// $FF      1
    REG_1    : Byte     ;// $FF      1
    CNT_REM  : Byte     ;         // 1
    CNT_PER  : Byte     ;         // 1
    CRC      : Byte     ;         // 1
  end;                            // 9 SizeOf = 10 ????

  {TOWDS18S20}
  POWDS18X20 = ^TOWDS18X20                                                          ;
  POWDS1820  = ^TOWDS18X20                                                          ;
  POWDS18S20 = ^TOWDS18X20                                                          ;
  POWDS18B20 = ^TOWDS18X20                                                          ;

  { TOWDS18X20 }

  TOWDS18X20 = class(TOWDefaultDevice)
  private
    function    GetCurTempStr: String;
    function    GetMaxTemp               : ShortInt                                 ;
    function    GetMaxTempStr: String;
    function    GetMinTemp               : ShortInt                                 ;
    function    GetCurTemp               : Integer                                  ;
    function    GetMinTempStr: String;
    function    GetResolut               : Byte                                     ;
    procedure   SetMaxTemp  (const AValue: ShortInt)                                ;
    procedure   SetMinTemp  (const AValue: ShortInt)                                ;
    procedure   SetResolut  (const AValue: Byte    )                                ;
  public
    constructor Create      (const AOWire: TOneWire                                 ;
                             const ADev  : TOWROM  )                                ;
    destructor  Destroy                  ; override                                 ;
    function    Convert                  : Boolean                                  ;
    function    SaveData                 : Boolean                                  ;
    function    LoadData                 : Boolean                                  ;
    function    SaveEEPROM               : Boolean                                  ;
    function    LoadEEPROM               : Boolean                                  ;
    function    Busy                     : Boolean                                  ;
    property    CurTemp                  : Integer  read GetCurTemp                 ;
    property    CurTempStr               : String   read GetCurTempStr              ;
    property    MaxTemp                  : ShortInt read GetMaxTemp    write SetMaxTemp;
    property    MaxTempStr               : String   read GetMaxTempStr;
    property    MinTemp                  : ShortInt read GetMinTemp    write SetMinTemp;
    property    MinTempStr               : String   read GetMinTempStr;
    property    Resolution               : Byte     read GetResolut    write SetResolut;
  end                                                                               ;

  TOWDS1820  = TOWDS18X20;
  TOWDS18S20 = TOWDS18X20;
  TOWDS18B20 = TOWDS18X20;


implementation

{ TOWDS18S20 }

function    TOWDS18X20.GetCurTemp             : Integer  ;
begin
  Result:= -85;
  if not(Assigned(fDATA)) then Exit;
  if TDS18X20(fDATA^).CNT_PER = 0 then Exit;
  if TypeDevice = $28
  then Result := Trunc(TDS18X20(fDATA^).TEMP / 16.0 * 100)
  else Result := Trunc((TDS18X20(fDATA^).TEMP / 2 - 0.25 + (TDS18X20(fDATA^).CNT_PER - TDS18X20(fDATA^).CNT_REM) / TDS18X20(fDATA^).CNT_PER) * 100);
end;

function    TOWDS18X20.GetMinTempStr          : String;
begin
  Result:= IntToStr(MinTemp) + fMEASURE;
end;

function    TOWDS18X20.GetResolut             : Byte     ;
begin
  Result:= 3;
  if not(Assigned(fDATA)) then Exit;
  Result:= not($80)and(TDS18X20(fDATA^).CONF shr 6); // Выделяем разрядность датчика.
end;

function    TOWDS18X20.GetCurTempStr          : String;
begin
  Result:= IntToStr(GetCurTemp div 100)+ DefaultFormatSettings.DecimalSeparator +IntToStr(GetCurTemp mod 100) + fMEASURE;
end;

function    TOWDS18X20.GetMaxTemp             : ShortInt ;
begin
  Result:= -85;
  if not(Assigned(fDATA)) then Exit;
  Result:= TDS18X20(fDATA^).TEMP_MAX;
end;

function    TOWDS18X20.GetMaxTempStr          : String;
begin
  Result:= IntToStr(MaxTemp) + fMEASURE;
end;

function    TOWDS18X20.GetMinTemp             : ShortInt ;
begin
  Result:= -85;
  if not(Assigned(fDATA)) then Exit;
  Result:= TDS18X20(fDATA^).TEMP_MIN;
end;

procedure   TOWDS18X20.SetMaxTemp(const AValue: ShortInt);
begin
  if not(Assigned(fDATA)) then Exit;
  TDS18X20(fDATA^).TEMP_MAX:= AValue;
end;

procedure   TOWDS18X20.SetMinTemp(const AValue: ShortInt);
begin
  if not(Assigned(fDATA)) then Exit;
  TDS18X20(fDATA^).TEMP_MAX:= AValue;
end;

procedure   TOWDS18X20.SetResolut(const AValue: Byte    );
begin
  if not(Assigned(fDATA)) then Exit;
  TDS18X20(fDATA^).CONF:= AValue shl 6;
end;

constructor TOWDS18X20.Create    (const AOWire: TOneWire; const ADev: TOWROM);
begin
  inherited Create(AOWire, ADev);
end;

destructor  TOWDS18X20.Destroy                           ;
begin
  inherited Destroy;
end;

function    TOWDS18X20.Convert                : Boolean  ;
var
  tim : Integer = 200;
begin
  Result:= false;
  if not(Bus.Select(fROM    )) then Exit;
  if not(Bus.Write(CONVERT_T)) then Exit;              // Начинаем измерение температуры
  if not(Power) then
  begin
    if ((MAC[5] * 256 + MAC[4]) >= 8) then
    case Resolution of
      0: tim:= 94;
      1: tim:= 188;
      2: tim:= 375;
      3: tim:= 750;
    end;
    Bus.BusyTime:= tim;                                // Запрещаем работу на шине
  end;
  Result:= true;
end;

function    TOWDS18X20.SaveData               : Boolean  ;
begin
  Result:= false;
  if not(Bus.Select(fROM    )) then Exit;
  if not(Assigned(fDATA)) then Exit;
  if not(Bus.Write(WRITE_DATA)) then Exit;
  if not(Bus.Write(TDS18X20(fDATA^).TEMP_MAX  )) then Exit;
  if not(Bus.Write(TDS18X20(fDATA^).TEMP_MIN  )) then Exit;
  if TypeDevice = $28 then
    if not(Bus.Write(TDS18X20(fDATA^).CONF    )) then Exit;
  Result:= true;
end;

function    TOWDS18X20.LoadData               : Boolean  ;
var ind   : Integer = 0;
begin
  Result  := false;
  if not(Bus.Select(fROM    )) then Exit;
  if not(Assigned(fDATA)) then GetMem(fDATA, SizeOf(PDS18X20));
  if not(Bus.Write(READ_DATA)) then Exit;
  for ind := 0 to Pred(SizeOf(TDS18X20) - 1) do
    if not(Bus.Read(PByte(fDATA)[ind])) then Exit;
  Result  := CheckCRC(fDATA^, Pred(SizeOf(TDS18X20)));
end;

function    TOWDS18X20.SaveEEPROM             : Boolean  ;
begin
 Result   := false;
 if not(Bus.Select(fROM    )) then Exit;
 if not(Bus.Write(WRITE_EEPROM)) then Exit;
 if not(Power) then Bus.BusyTime:= 20; // Занимаем шину
 Result   := true;
end;

function    TOWDS18X20.LoadEEPROM             : Boolean  ;
begin
  Result  := true;
  if not(Bus.Select(fROM    )) then Exit;
  if not(Bus.Write(READ_EEPROM)) then Exit;
  if not(Power) then Bus.BusyTime:= 10; // Занимаем шину
  Result  := true;
end;

function    TOWDS18X20.Busy                   : Boolean  ;
begin
  Result  := true;
  if Power then
    if not(Bus.ReadBit(Result)) then Result:= true;
  Result:= not(Result);
end;

end.

