unit DS18X20;

{$mode objfpc}{$H+}
{$DEFINE DEBUG}
interface

uses
  SysUtils, OneWire, ttyUSB
  {$IFDEF DEBUG}
   , StrUtils
  {$ENDIF};

const

   CONVERT_T         = $44; // Конвертировать температуру
   WRITE_SCRATCHPAD  = $4E; // Записать данные
   READ_SCRATCHPAD   = $BE; // Прочитать данные
   COPY_SCRATCHPAD   = $48; // Копировать настройки в ЕЕ
   RECALL_E2         = $B8; // Копировать настройки из ЕЕ

   bit_09            = $00;
   bit_10            = $01;
   bit_11            = $02;
   bit_12            = $03;

var
   MAX_BUSY_TIME     : Integer;

type

  { TDS18X20 }
  TDS18X20 = record
    sTEMP     : SmallInt;
    sTEMP_MAX : ShortInt;
    sTEMP_MIN : ShortInt;
    sREG_0    : Byte;
    sREG_1    : Byte;
    sREG_2    : Byte;
    sREG_3    : Byte;
    sCRC      : Byte;
  end;

  { TOWTermo }

  TOWTermo = class(TObject)
  private
    fOneWire                             : TOneWire                                         ;
    fROM                                 : TOWDEV                                           ; // тип устройства, его МАС и контрольная сумма
    fDAT                                 : TDS18X20                                         ; // Данные полученные от устройства и/или предназначенные для записи в устройство
    fTIM                                 : Integer                                          ; // Время ожидания операции измерения температуры
    function    GetRes                   : Byte                                             ;
    function    GetTemp                  : Integer                                          ;
    procedure   SetRes(AValue            : Byte)                                            ;
  public
    constructor Create(const AOneWire: TOneWire; const aDEV : TOWDEV)                       ;
    destructor  Destroy                  ; override                                         ;
    function    Convert                  : Boolean                                          ;
    function    TypeName                 : String;
    function    MAC                      : String;
    function    SaveData                 : Boolean                                          ;
    function    LoadData                 : Boolean                                          ;
    function    SaveEE                   : Boolean                                          ;
    function    LoadEE                   : Boolean                                          ;
    function    Busy                     : Boolean                                          ;
    property    TypeDev                  : Byte     read fROM.DEV_TYPE                      ;
    property    CurTemp                  : Integer  read GetTemp                            ;
    property    MaxTemp                  : ShortInt read fDAT.sTEMP_MAX write fDAT.sTEMP_MAX;
    property    MinTemp                  : ShortInt read fDAT.sTEMP_MIN write fDAT.sTEMP_MIN;
    property    Resolut                  : Byte     read GetRes     write SetRes            ;
  end;

implementation

{ TOWTermo }

function    TOWTermo.GetRes                                             : Byte      ;
begin
  Result:= 3;
  if fROM.DEV_TYPE = $10 then Exit; // Если датчик не поддерживает разрядность, то выходим.
  if fDAT.sREG_0 <> 0 then Result:= not($80)and(fDAT.sREG_0 shr 5); // Выделяем разрядность датчика.
  {$IFDEF DEBUG}
    Writeln('Получили флаги конфигурации:   ' + IntToBin(fDAT.sREG_0, 8));
  {$IFEND}
end;

function    TOWTermo.GetTemp                                            : Integer   ;
begin
  if fROM.DEV_TYPE = $28
  then Result := Trunc(fDAT.sTEMP / 16.0 * 100)
  else Result := Trunc((fDAT.sTEMP / 2 - 0.25 + (fDAT.sREG_3 - fDAT.sREG_2) / fDAT.sREG_3) * 100);
end;

procedure   TOWTermo.SetRes(AValue: Byte)                                           ;
begin
  if fROM.DEV_TYPE = $10 then Exit;
  if AValue < 4 then fDAT.sREG_0:= AValue shl 5;
  {$IFDEF DEBUG}
    Writeln('Установили флаги конфигурации: ' + IntToBin(fDAT.sREG_0, 8));
  {$IFEND}
end;

constructor TOWTermo.Create(const AOneWire: TOneWire; const aDEV: TOWDEV)           ;
begin
  case ADEV.DEV_TYPE of
  $10 :
     begin
      inherited Create;
      MAX_BUSY_TIME:= 200;
      if ((ADEV.DEV_MAC[5] * 256 + ADEV.DEV_MAC[4]) >= 8) then MAX_BUSY_TIME:= 750;
      fOneWire := AOneWire;
      fROM     := ADEV;
      fTIM     := MAX_BUSY_TIME;
     end;
  $28 :
    begin
      inherited Create;
      MAX_BUSY_TIME:= 750;
      fOneWire := AOneWire;
      fROM     := ADEV;
      fTIM     := MAX_BUSY_TIME;
    end
  else Exit;
  end;

end;

destructor  TOWTermo.Destroy                                                        ;
begin
   inherited Destroy;
end;

function    TOWTermo.Convert                                            : Boolean   ;
begin
  Result:= false;
  fOneWire.WaitBus; // Ожидаем шину
  if not(WriteTTY(fOneWire.Handle, CONVERT_T)) then Exit; // Начинаем измерение температуры
  if not(fOneWire.Power) then fOneWire.BusyTime:= fTIM;
  Result:= true;
  {$IFDEF DEBUG}
    Writeln('Измерение температуры (', fTIM, ' msec) ');
  {$IFEND}
end;

function    TOWTermo.LoadData                                           : Boolean   ;
var ind : Integer = 0;
    tmp : TDS18X20   ;
begin
  Result  := false;
  fOneWire.WaitBus; // Ожидаем шину
  if not(WriteTTY(fOneWire.Handle, READ_SCRATCHPAD)) then Exit;
  for ind := 1 to Pred(SizeOf(tmp)) do
    if not(ReadTTY(fOneWire.Handle, PByte(@tmp)[Pred(ind)])) then Exit;
  if CheckCRC(tmp, Pred(SizeOf(tmp))) then fDAT  := tmp;
  case Resolut of
    0: fTIM:= MAX_BUSY_TIME div 8;
    1: fTIM:= MAX_BUSY_TIME div 4;
    2: fTIM:= MAX_BUSY_TIME div 2;
  else fTIM:= MAX_BUSY_TIME;
  end;
  Result:= true;
end;

function    TOWTermo.SaveEE                                             : Boolean   ;
begin
 Result:= false;
 fOneWire.WaitBus; // Ожидаем шину
 if not(WriteTTY(fOneWire.Handle, COPY_SCRATCHPAD)) then Exit;
 if not(fOneWire.Power) then fOneWire.BusyTime:= 20; // Занимаем шину
 Result:= true;
end;

function    TOWTermo.LoadEE                                             : Boolean   ;
begin
  Result:= true;
  fOneWire.WaitBus; // Ожидаем шину
  if not(WriteTTY(fOneWire.Handle, RECALL_E2)) then Exit;
  if (fOneWire.Power) then fOneWire.BusyTime:= 10; // Занимаем шину
  Result:= true;
end;

function    TOWTermo.Busy                                               : Boolean   ;
begin
  Result:= false;
  fOneWire.WaitBus; // Ожидаем шину
  if fOneWire.Power then
  begin
    r_bit(fOneWire.Handle, Result);
    Result:= not(Result);
  end;
  {$IFDEF DEBUG}
   if Result then Write('-');
  {$IFEND}
end;

function    TOWTermo.TypeName                                           : String    ;
begin
  case fROM.DEV_TYPE of
  $10: if (fROM.DEV_MAC[5] * 256 + fROM.DEV_MAC[4]) >= 8
        then Result:='DS18S20'
        else Result:='DS1820 ';
  $28:       Result:='DS18B20';
  end;
end;

function    TOWTermo.MAC                                                : String    ;
begin
  Result:= IntToHEX(fROM.DEV_MAC[0], 2) + '-' + IntToHEX(fROM.DEV_MAC[1], 2) + '-' +
           IntToHEX(fROM.DEV_MAC[2], 2) + '-' + IntToHEX(fROM.DEV_MAC[3], 2) + '-' +
           IntToHEX(fROM.DEV_MAC[4], 2);
end;

function    TOWTermo.SaveData                                           : Boolean   ;
begin
  Result:= false;
  fOneWire.WaitBus; // Ожидаем шину
  if not(WriteTTY(fOneWire.Handle, WRITE_SCRATCHPAD)) then Exit;
  if not(WriteTTY(fOneWire.Handle, fDAT.sTEMP_MAX  )) then Exit;
  if not(WriteTTY(fOneWire.Handle, fDAT.sTEMP_MIN  )) then Exit;
  if fROM.DEV_TYPE = $28 then
    if not(WriteTTY(fOneWire.Handle, fDAT.sREG_0    )) then Exit;
  Result:= true;
end;

end.

