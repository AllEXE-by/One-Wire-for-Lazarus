unit OneWire;

{╔═══════════════════════════════════════════════════════════════════════════════╗
 ║                                                                               ║
 ║                   ███╗     ██╗       ██╗██╗██████╗ ███████╗                   ║
 ║                  ████║     ██║  ██╗  ██║██║██╔══██╗██╔════╝                   ║
 ║                 ██╔██║     ╚██╗████╗██╔╝██║██████╔╝█████╗                     ║
 ║                 ╚═╝██║      ████╔═████║ ██║██╔══██╗██╔══╝                     ║
 ║                 ███████╗    ╚██╔╝ ╚██╔╝ ██║██║  ██║███████╗                   ║
 ║                 ╚══════╝     ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝                   ║
 ║                                                                               ║
 ║           ╔═══╗╔═══╗╔═══╗     ╔╗   ╔═══╗╔════╗╔═══╗╔═══╗╔╗ ╔╗╔═══╗            ║
 ║           ║╔══╝║╔═╗║║╔═╗║     ║║   ║╔═╗║╚══╗ ║║╔═╗║║╔═╗║║║ ║║║╔═╗║            ║
 ║           ║╚══╗║║ ║║║╚═╝║     ║║   ║║ ║║  ╔╝╔╝║║ ║║║╚═╝║║║ ║║║╚══╗            ║
 ║           ║╔══╝║║ ║║║╔╗╔╝     ║║ ╔╗║╚═╝║ ╔╝╔╝ ║╚═╝║║╔╗╔╝║║ ║║╚══╗║            ║
 ║           ║║   ║╚═╝║║║║╚╗     ║╚═╝║║╔═╗║╔╝ ╚═╗║╔═╗║║║║╚╗║╚═╝║║╚═╝║            ║
 ║           ╚╝   ╚═══╝╚╝╚═╝     ╚═══╝╚╝ ╚╝╚════╝╚╝ ╚╝╚╝╚═╝╚═══╝╚═══╝            ║
 ║                                                                               ║
 ║  Copyright (C)               2021, Alexei NUZHKOV, <alexeidg@tut.by>, et al.  ║
 ║  Авторское право (С)         2021, Алексей НУЖКОВ и другие.                   ║
 ║                                                                               ║
 ║  Данное программное обеспечение лицензировано MIT.                            ║
 ║  Условия доступны по адресу:                                                  ║
 ║                                                                               ║
 ║  Вы можете использовать, копировать, изменять, объединять, публиковать,       ║
 ║  распространять и/или продавать копии программного обеспечения                ║
 ║  в соответствии с условиями:                                                  ║
 ║                                                                               ║
 ║  Это программное обеспечение распространяется на условиях "КАК ЕСТЬ",         ║
 ║  БЕЗ каких либо ГАРАНТИЙ, явных или подразумеваемых.                          ║
 ╚═══════════════════════════════════════════════════════════════════════════════╝}

{$mode objfpc}{$H+}{$DEFINE NO_DEBUG}
interface

uses
  SysUtils, ttyUSB;

function    CheckCRC   (const ABuffer; const ASize : Integer): Boolean                              ;

type
  { TOWDEV }
  POWDeviceRom        = ^TOWDeviceRom                                                               ;
  TOWDeviceRom        = record
    DEV_TYPE                               : Byte                                                   ;
    DEV_MAC                                : Array[0..5] of Byte                                    ;
    DEV_CRC                                : Byte                                                   ;
  end;

  { TONEWIRE }
  POneWire            = ^TOneWire                                                                   ;
  TOneWire            = class(TObject)
    private
      fHandle                                              : THandle                                ;
      fEROM                                                : Integer                                ;                           // Позиция последнего несоответствия ROM.
      fENDD                                                : Boolean                                ;                        // Признак последнего устройства
      fBUSY                                                : Integer                                ;
      fDEVICES                                             : Array of TOWDeviceROM                  ;
      fDEVICESALARM                                        : Array of TOWDeviceROM                  ;
      function    SearchEx      (var   ADevice : TOWDeviceRom): Boolean                             ;
      function    Search        (var   ADevice : TOWDeviceRom): Boolean                             ;
      function    SearchAlarm   (var   ADevice : TOWDeviceRom): Boolean                             ;

      function    GetDevice     (Index : Integer)          : TOWDeviceRom                           ;
      function    GetAlarmDevice(Index : Integer)          : TOWDeviceRom                           ;
      function    GetDeviceCount                           : Integer                                ;
      function    GetDeviceAlarmCount                      : Integer                                ;
      function    GetBusy                                  : Int64                                  ;
      procedure   SetBusy       (const AValue  : Int64)                                             ;
      function    GetHandle                                : THandle                                ;
    public
      constructor Create                                                                            ;
      destructor  Destroy; override                                                                 ;
      function    Connect       (const aPort : String)        : Boolean                             ;
      function    Reset                                    : Boolean                                ;
      function    GetPower                                 : Boolean                                ;
      procedure   WaitBus                                                                           ;
      function    SelectAll                                : Boolean                                ;
      function    Select        (const ADevice : TOWDeviceRom): Boolean                             ;
      function    ReadROM       (var   ADevice : TOWDeviceRom): Boolean                             ;
      procedure   SearchAll                                                                         ;
      procedure   SearchAllAlarm                                                                    ;
      procedure   SearchClear                                                                       ;
      property    Handle                                   : THandle      read GetHandle            ;
      property    BusyTime                                 : Int64        read GetBusy write SetBusy;
      property    Devices[Index : Integer]                 : TOWDeviceRom read GetDevice            ;
      property    DevicesCount                             : Integer      read GetDeviceCount       ;
      property    DevicesAlarm[Index : Integer]            : TOWDeviceRom read GetAlarmDevice       ;
      property    DevicesAlarmCount                        : Integer      read GetDeviceAlarmCount  ;
  end                                                                                               ;


    { TOWDefaultDevice }
  POWDefaultDevice    = ^TOWDefaultDevice                                                           ;
  TOWDefaultDevice    = class(TObject)
    private
      fOneWire                             : TOneWire                                               ;
      fDATA                                : Pointer                                                ; // Данные полученные от устройства и/или предназначенные для записи в устройство
    public
      constructor Create     (const AOWire : POneWire; const AROM : POWDeviceRom)                   ;
      destructor  Destroy; override                                                                 ;
  end;


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
   SEARCH_ALR        = $EC; // Искать МАС устройств с установленной тревогой
   READ_POWER_SUPPLY = $B4; // Читать состояние питания


implementation


function    CheckCRC   (const ABuffer; const ASize  : Integer): Boolean   ;
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

{ TOWDefaultDevice }

constructor TOWDefaultDevice.Create(const AOWire : POneWire; const AROM : POWDeviceRom);
begin
  inherited Create                                                                  ;
    fOneWire                             := @AOWire                                  ;
    fROM                                 := @AROM                                    ; // тип устройства, его МАС и контрольная сумма
    fDATA                                := nil                                     ; // Данные полученные от устройства и/или предназначенные для записи в устройство

end;

destructor TOWDefaultDevice.Destroy;
begin
  inherited Destroy                                                                 ;
end;

{ TOneWire }

function    TOneWire.Reset                              : Boolean   ;
var res : Byte                                                      ;
begin
  Result:= false                                                    ;
  if not(OWConf(fHandle, false)) then Exit                        ;
  res:= $F0                                                         ;
  if not(OWWriteReadBit(fHandle, res))  then Exit;
  if not(OWConf(fHandle))then Exit                                ;
  BusyTime:= 0;                                          ;
  Result:=    (res <> $F0)                                          ;
end                                                                 ;

function    TOneWire.GetPower                    : Boolean   ;
var res: Boolean = false;
begin
  Result:= false;
  if not(OWWrite(fHandle, READ_POWER_SUPPLY)) then Exit;
  if not(OWReadBit(fHandle, res)) then Exit;
  Result:= true;
end;

procedure   TOneWire.WaitBus;
{$IFDEF DEBUG}
var r : Boolean = false;
{$IFEND}
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
  {$IFDEF DEBUG}
  if r then Writeln('');
  if r then Writeln('ШИНА ДОСТУПНА');
  {$IFEND}
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

function TOneWire.GetAlarmDevice(Index : Integer): TOWDeviceRom;
begin
  Result:= fDEVICESALARM[Index];
end;

function TOneWire.GetDevice(Index: Integer): TOWDeviceRom;
begin
  Result:= fDEVICES[Index];
end;

function TOneWire.GetDeviceAlarmCount: Integer;
begin
  Result:= Length(fDEVICESALARM);
end;

function TOneWire.GetDeviceCount: Integer;
begin
  Result:= Length(fDEVICES);
end;

function    TOneWire.SelectAll                                            : Boolean   ;
begin
  Result:= false;
  WaitBus;
  if not(Reset) then Exit;
  if not(OWWrite(fHandle, SKIP_ROM)) then Exit;
  Result:= true;
end;

function    TOneWire.Select     (const ADevice : TOWDeviceRom)                  : Boolean   ;
var ind : Integer;
begin
  Result:= false;
  WaitBus;
  if not(Reset) then Exit;
  if not(OWWrite(fHandle, MATCH_ROM)) then Exit;
  for ind:= 0 to 7 do if not(OWWrite(fHandle, PByte(@ADevice)[ind])) then Exit;
  Result:= true;
end;

function    TOneWire.ReadROM    (var   ADevice  : TOWDeviceRom)                  : Boolean   ;
var ind : Integer = 0;
begin
  Result:= false;
  WaitBus;
  if not(Reset) then Exit;
  if not(OWWrite(fHandle, READ_ROM)) then Exit;
  for ind:= 0 to 7 do if not(OWRead(fHandle, PByte(@ADevice)[ind])) then Exit; // Family
  Result:= true;
end;

function    TOneWire.Search     (var   ADevice  : TOWDeviceRom)                  : Boolean   ;
begin
  Result:= false;
  WaitBus;
  if not(Reset                                ) then Exit;
  if not(OWWrite(fHandle, SEARCH_ROM         )) then Exit;
  if not(SearchEx   (ADevice                 )) then Exit;
  Result:= true;
end;

procedure TOneWire.SearchAll;
var
  SearchROM : TOWDeviceRom;
begin
  SetLength(fDEVICES, 0);
  if Self.Reset then
    while Self.Search(SearchROM) do
    begin
      SetLength(fDEVICES, Length(fDEVICES) + 1);
      fDEVICES[Length(fDEVICES) - 1]:= SearchROM;
    end;
  Self.Reset;
end;

function    TOneWire.SearchAlarm(var   ADevice   : TOWDeviceRom)                  : Boolean   ;
begin
  Result:= false;
  WaitBus;
  if not(Reset                               ) then Exit;
  if not(OWWrite(fHandle, SEARCH_ALR        )) then Exit;
  if not(SearchEx   (ADevice                )) then Exit;
  Result:= true;
end;

procedure TOneWire.SearchAllAlarm;
var
  SearchROM : TOWDeviceRom;
begin
  SetLength(fDEVICESALARM, 0);
  if Self.Reset then
    while Self.SearchAlarm(SearchROM) do
    begin
      SetLength(fDEVICESALARM, Length(fDEVICESALARM) + 1);
      fDEVICESALARM[Length(fDEVICESALARM) - 1]:= SearchROM;
    end;
  Self.Reset;
end;

procedure   TOneWire.SearchClear                                                    ;
begin
  fEROM        := 0                ;
  fENDD        := false            ;
end;

function    TOneWire.SearchEx   (var   ADevice   : TOWDeviceRom)                  : Boolean   ;
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
      if not(OWReadBit(fHandle, a_bit)) then Exit;                             // Читаем 1-ый бит
      if not(OWReadBit(fHandle, b_bit)) then Exit;                             // Читаем 2-ой бит
      if (a_bit and b_bit)                                              // Если биты установлены...
        then Exit                                                       // Выходим
        else
        begin
          if (a_bit <> b_bit)                                           // все соединенные устройства вернули 0 или 1
          then rom_search:= a_bit                                     // значение записи бита для поиска
          else
          begin
            if (ind_bit < fEROM)                                      // если это расхождение, если перед Последним расхождением
              then rom_search:= ((PByte(@ADevice)[ind_byte] and rom_byte) > 0) // на предыдущем следующем затем выберите то же самое, что и в прошлый раз
              else rom_search:= (ind_bit = fEROM);                    // если равно последнему выбору 1, если нет, то выберите 0
            if not(rom_search) then                                   // если был выбран 0, то запишите его позицию в LastZero
            begin
              last_zero:= ind_bit;                                    // проверьте наличие последнего несоответствия в семье
              if (last_zero < 9) then fEROM:= last_zero;              // &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
            end;
          end;
          if (rom_search)                                             // установите или очистите бит в байте ROM rom_byte_number
          then PByte(@ADevice)[ind_byte]:= PByte(@ADevice)[ind_byte] or rom_byte        // с маской rom_byte_mask
          else PByte(@ADevice)[ind_byte]:= PByte(@ADevice)[ind_byte] and not(rom_byte);
          if not(OWWriteBit(fHandle, rom_search)) then Exit;                  //серийный номер направление поиска бит записи
          inc(ind_bit);                                               // увеличить счетчик байтов id_bit_number
          rom_byte := rom_byte shl 1;                                 // и сдвинуть маску rom_byte_mask
          if (rom_byte = 0) then                                      // если маска равна 0 то перейдите в new SerialNum byte rom_byte_number и сбросьте маску
          begin
            inc(ind_byte);
           rom_byte:= 1;
          end;
        end;
    until (ind_byte = 8);                                             // цикл до тех пор пока через все байты ПЗУ 0-7
    if ((ind_bit > 64) or (PByte(@ADevice)[7] <> 0)) then                        // если поиски увенчались успехом, то
    begin
      fEROM := last_zero;                                               // поиск успешен,поэтому установите LastDiscrepancy, LastDeviceFlag,search_result
      if (fEROM = 0) then fENDD := true;                                // нашли последнее устройство
      Result:= true;
    end;
  end;
  if (not(Result) or (PByte(@ADevice)[0] = 0)) then                         // если устройство не найдено, то сбросьте счетчики, так что следующий "поиск" будет похож на первый
  begin
    fEROM := 0;
    fENDD := false;                                                      // Флаг последнего найденного устройства
    Result:= false;
  end else Result:= CheckCRC(ADevice, SizeOf(ADevice));
end;

procedure   TOneWire.SetBusy    (const AValue : Int64);
begin
  fBUSY:= GetTickCount64 + AValue;
end;

constructor TOneWire.Create;
begin
  inherited Create                           ;
  fBUSY        := GetTickCount64             ;
  fEROM        := 0                          ;
  fENDD        := false                      ;
end;

destructor  TOneWire.Destroy                                                        ;
begin
  OWClose(fHandle);
  inherited Destroy;
end;

function TOneWire.Connect(const aPort: String): Boolean;
begin
  Result:= false                                       ;
  fHandle      := OWOpen(APort)                        ;
  Result:= fHandle <> OW_INVALID_HANDLE                ;
  if Result then
  begin
    Self.SearchAll                                     ;
    Self.SearchAllAlarm                                ;
  end;
end;

end.
