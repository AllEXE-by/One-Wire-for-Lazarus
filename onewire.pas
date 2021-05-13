unit OneWire;

{$mode objfpc}{$H+}
{$DEFINE DEBUG}
interface

uses
  SysUtils, ttyUSB;



function    CheckCRC   (const ABuffer          ; const ASize  : Integer): Boolean;

type

  { TOWDEV }
  TOWDEV        = record
    DEV_TYPE  : Byte;
    DEV_MAC   : Array[0..5] of Byte;
    DEV_CRC   : Byte;
  end;
  POWDEV = ^TOWDEV;


  { TONEWIRE }
  TOneWire = class(TObject)
    private
      fHandle                                        : THandle                         ;
      fEROM                                          : Integer                         ;                           // Позиция последнего несоответствия ROM.
      fENDD                                          : Boolean                         ;                        // Признак последнего устройства
      fBUSY                                          : Integer                         ;
      fPOWER                                         : Boolean                         ;
      function    GetBusy                            : Int64;
      function    GetHandle                          : THandle                         ;
      function    SearchEx    (var   ADEV : TOWDEV)  : Boolean                         ;
      procedure   SetBusy     (const AValue: Int64);
    public
      constructor Create      (const ADevice : String)                                  ;
      destructor  Destroy; override                                                 ;
      function    Reset                              : Boolean                         ;
      function    GetPower                           : Boolean                         ;
      procedure   WaitBus;
      function    Power                              : Boolean                         ;
      function    SelectAll                          : Boolean                         ;
      function    Select      (const ADEV : TOWDEV  ): Boolean                         ;
      function    ReadROM     (var   ADEV : TOWDEV  ): Boolean                         ;
      function    Search      (var   ADEV : TOWDEV  ): Boolean                         ;
      function    SearchAlarm (var   ADEV : TOWDEV  ): Boolean                         ;
      procedure   SearchClear;
      property    Handle                             : THandle read GetHandle          ;
      property    BusyTime                           : Int64 read GetBusy write SetBusy ;
  end                                                                               ;



  {
  $01: Result:='DS1990A';
  $02: Result:='DS1991L';
  $04: Result:='DS1994L';
  $06: Result:='DS1993L';
  $08: Result:='DS1992L';
  $09: Result:='DS1982' ;
  $0A: Result:='DS1995L';
  $0B: Result:='DS1985' ;
  $0C: Result:='DS1996L';
  $0F: Result:='DS1986' ;
  *$10: if (fROM.ROM_MAC[5] * 256 + fROM.ROM_MAC[4]) >= 8
        then Result:='DS18S20'
        else Result:='DS1820';
  $12: Result:='DS2604' ;
  $1A: Result:='DS1963L';
  $21: Result:='DS1921' ;
  $22: Result:='DS1822' ;
  *$28: Result:='DS18B20';
  $33: Result:='DS1961S';
  $41: Result:='DS1922L';
  $89: Result:='DS1982U';
  $8B: Result:='DS1985U';
  $8F: Result:='DS1986U';
}


 const
   // КОМАНДЫ ONE-WIRE
   SKIP_ROM          = $CC; // Игнорировать МАС устройства
   MATCH_ROM         = $55; // Выбрать устройство по МАС
   READ_ROM          = $33; // Прочитать МАС устройства
   SEARCH_ROM        = $F0; // Искать МАС устройств
   ALARM_SEARCH      = $EC; // Искать МАС устройств с установленной тревогой
   READ_POWER_SUPPLY = $B4; // Читать состояние питания


implementation


function    CheckCRC   (const ABuffer          ; const ASize  : Integer): Boolean   ;
var
  ind_0        : Integer = 0                                                        ;
  ind_1        : Integer = 0                                                        ;
  tmp          : Byte    = 0                                                        ;
  fCRC         : Byte    = 0                                                        ;
begin
  for ind_0    := 0 to ASize - 2 do
  begin
  tmp          := PByte(@ABuffer)[ind_0]                                            ;
  for ind_1    := 1 to 8 do
  begin
   if Odd(tmp xor fCRC)
    then fCRC  := ((fCRC xor $18) shr 1) or $80
    else fCRC  := fCRC shr 1                                                        ;
  tmp          := tmp shr 1                                                         ;
  end                                                                               ;
  end                                                                               ;
  Result       := fCRC = PByte(@ABuffer)[Pred(ASize)]                               ;
end                                                                                 ;

{ TOneWire }

function    TOneWire.Reset                              : Boolean   ;
var res : Byte                                                      ;
begin
  Result:= false                                                    ;
  if not(ConfTTY(fHandle, false)) then Exit                        ;
  res:= $F0                                                         ;
  if not(wr_bit(fHandle, res))  then Exit;
  if not(ConfTTY(fHandle))then Exit                                ;
  BusyTime:= 0;                                          ;
  Result:=    (res <> $F0)                                          ;
end                                                                 ;

function    TOneWire.GetPower                    : Boolean   ;
var res: Boolean = false;
begin
  Result:= false;
  if not(WriteTTY(fHandle, READ_POWER_SUPPLY)) then Exit;
  if not(r_bit(fHandle, res)) then Exit;
  fPOWER:= res;
  Result:= true;
end;

procedure   TOneWire.WaitBus;
{$IFDEF DEBUG}
var r : Boolean = false;
{$IFEND}
begin
  if not(fPOWER) then
  begin
    {$IFDEF DEBUG}
    if BusyTime > 0 then Writeln('ШИНА НЕ ДОСТУПНА (паразитное питание)');
    {$IFEND}
    while BusyTime > 0 do
    begin
      Sleep(10);
      {$IFDEF DEBUG}
        Write('.');
        r:= true;
      {$IFEND}
    end;
  end;
  {$IFDEF DEBUG}
  if r then Writeln('');
  if r then Writeln('ШИНА ДОСТУПНА');
  {$IFEND}
end;

function    TOneWire.Power: Boolean;
begin
  Result:= fPOWER;
end;

function    TOneWire.GetHandle                                          : THandle   ;
begin
  Result:= fHandle                                                  ;
end                                                                 ;

function    TOneWire.GetBusy: Int64;
begin
  Result:= fBUSY - GetTickCount64;
  if Result < 0 then Result:= 0;
end;

function    TOneWire.SelectAll                                            : Boolean   ;
begin
  Result:= false;
  WaitBus;
  if not(Reset) then Exit;
  if not(WriteTTY(fHandle, SKIP_ROM)) then Exit;
  Result:= true;
end;

function    TOneWire.Select     (const ADEV   : TOWDEV  )                  : Boolean   ;
var ind : Integer;
begin
  Result:= false;
  WaitBus;
  if not(Reset) then Exit;
  if not(WriteTTY(fHandle, MATCH_ROM)) then Exit;
  for ind:= 0 to 7 do if not(WriteTTY(fHandle, PByte(@ADEV)[ind])) then Exit;
  Result:= true;
end;

function    TOneWire.ReadROM    (var   ADEV   : TOWDEV  )                  : Boolean   ;
var ind : Integer = 0;
begin
  Result:= false;
  WaitBus;
  if not(Reset) then Exit;
  if not(WriteTTY(fHandle, READ_ROM)) then Exit;
  for ind:= 0 to 7 do if not(ReadTTY(fHandle, PByte(@ADEV)[ind])) then Exit; // Family
  Result:= true;
end;

function    TOneWire.Search     (var   ADEV   : TOWDEV  )                  : Boolean   ;
begin
  Result:= false;
  WaitBus;
  if not(Reset                           ) then Exit;
  if not(WriteTTY(fHandle, SEARCH_ROM  )) then Exit;
  if not(SearchEx   (ADEV                 )) then Exit;
  Result:= true;
end;

function    TOneWire.SearchAlarm(var   ADEV   : TOWDEV  )                  : Boolean   ;
begin
  Result:= false;
  WaitBus;
  if not(Reset                           ) then Exit;
  if not(WriteTTY(fHandle, ALARM_SEARCH)) then Exit;
  if not(SearchEx   (ADEV                 )) then Exit;
  Result:= true;
end;

procedure   TOneWire.SearchClear                                                    ;
begin
  fEROM        := 0                ;
  fENDD        := false            ;
end;

function    TOneWire.SearchEx   (var   ADEV   : TOWDEV  )                  : Boolean   ;
var ind_bit      : Byte = 1;                                         // Номер текущего бита байта адреса
    ind_byte     : Byte = 0;                                         // Номер текущего байта в адресе
    rom_byte     : Byte = 1;                                         //
    last_zero    : Byte = 0;                                         //
    a_bit        : Boolean = false;                                  //
    b_bit        : Boolean = false;                                  //
    rom_search   : Boolean;                                          //
begin
  Result:= false;                                                    // Результат - неудача.
  if not(fENDD) then
  begin
    repeat                                                              // Цикл поиска устройства.
      if not(r_bit(fHandle, a_bit)) then Exit;                             // Читаем 1-ый бит
      if not(r_bit(fHandle, b_bit)) then Exit;                             // Читаем 2-ой бит
      if (a_bit and b_bit)                                              // Если биты установлены...
        then Exit                                                       // Выходим
        else
        begin
          if (a_bit <> b_bit)                                           // все соединенные устройства вернули 0 или 1
          then rom_search:= a_bit                                     // значение записи бита для поиска
          else
          begin
            if (ind_bit < fEROM)                                      // если это расхождение, если перед Последним расхождением
              then rom_search:= ((PByte(@ADEV)[ind_byte] and rom_byte) > 0) // на предыдущем следующем затем выберите то же самое, что и в прошлый раз
              else rom_search:= (ind_bit = fEROM);                    // если равно последнему выбору 1, если нет, то выберите 0
            if not(rom_search) then                                   // если был выбран 0, то запишите его позицию в LastZero
            begin
              last_zero:= ind_bit;                                    // проверьте наличие последнего несоответствия в семье
              if (last_zero < 9) then fEROM:= last_zero;              // &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
            end;
          end;
          if (rom_search)                                             // установите или очистите бит в байте ROM rom_byte_number
          then PByte(@ADEV)[ind_byte]:= PByte(@ADEV)[ind_byte] or rom_byte        // с маской rom_byte_mask
          else PByte(@ADEV)[ind_byte]:= PByte(@ADEV)[ind_byte] and not(rom_byte);
          if not(w_bit(fHandle, rom_search)) then Exit;                  //серийный номер направление поиска бит записи
          inc(ind_bit);                                               // увеличить счетчик байтов id_bit_number
          rom_byte := rom_byte shl 1;                                 // и сдвинуть маску rom_byte_mask
          if (rom_byte = 0) then                                      // если маска равна 0 то перейдите в new SerialNum byte rom_byte_number и сбросьте маску
          begin
            inc(ind_byte);
           rom_byte:= 1;
          end;
        end;
    until (ind_byte = 8);                                             // цикл до тех пор пока через все байты ПЗУ 0-7
    if ((ind_bit > 64) or (PByte(@ADEV)[7] <> 0)) then                        // если поиски увенчались успехом, то
    begin
      fEROM := last_zero;                                               // поиск успешен,поэтому установите LastDiscrepancy, LastDeviceFlag,search_result
      if (fEROM = 0) then fENDD := true;                                // нашли последнее устройство
      Result:= true;
    end;
  end;
  if (not(Result) or (PByte(@ADEV)[0] = 0)) then                         // если устройство не найдено, то сбросьте счетчики, так что следующий "поиск" будет похож на первый
  begin
    fEROM := 0;
    fENDD := false;                                                      // Флаг последнего найденного устройства
    Result:= false;
  end else Result:= CheckCRC(ADEV, SizeOf(ADEV));
end;

procedure   TOneWire.SetBusy    (const AValue : Int64);
begin
  fBUSY:= GetTickCount64 + AValue;
end;

constructor TOneWire.Create     (const ADevice: String)                              ;
begin
  inherited Create                           ;
  fBUSY        := GetTickCount64             ;
  fPOWER       := false                      ;
  fEROM        := 0                          ;
  fENDD        := false                      ;
  fHandle      := OpenTTY(ADevice)           ;
  if fHandle    = INVALID_HANDLE then Destroy;
end;

destructor  TOneWire.Destroy                                                        ;
begin
  CloseTTY(fHandle);
  inherited Destroy;
end;

end.

