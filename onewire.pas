unit OneWire;

{$mode objfpc}{$H+}
{$DEFINE DEBUG}
interface

uses
  SysUtils, OneWireBus;

function    CheckCRC   (const ABuffer          ; const ASize  : Integer): Boolean;

type
  { TMAC }
  TMAC = Array[0..5] of Byte;

  { TOWDEV }
  POWROM = ^TOWROM;
  TOWROM      = record
    dTYPE  : Byte;
    dMAC   : TMAC;
    dCRC   : Byte;
  end;

  { TONEWIRE }
  TOneWire = class(TObject)
    private
      fBUS                                           : TOWBUS                          ;
      fEROM                                          : Integer                         ;
      fENDD                                          : Boolean                         ;
      fBUSY                                          : Integer                         ;
      fDEVICES                                       : Array of TOWROM                 ;
      fALARMS                                        : Array of TOWROM                 ;
      function    GetBusy                            : Int64                           ;
      function    GetDevice(index : Integer): TOWROM;
      function    GetDeviceAlarm(index : Integer): TOWROM;
      function    SearchEx    (var   ADevice : TOWROM)  : Boolean                   ;
      procedure   SetBusy     (const AValue: Int64)                                    ;
      function    SearchDevices      (var   ADevice : TOWROM   ): Boolean           ;
      function    SearchDevicesAlarm (var   ADevice : TOWROM   ): Boolean           ;
    public
      constructor Create      (const ADevice : String)                                 ;
      destructor  Destroy                   ;                                  override;
      function    Reset                               : Boolean                        ;
      function    Write       (const AValue : Byte   ): Boolean                        ;
      function    Read        (var   AValue : Byte   ): Boolean                        ;
      function    WriteBit    (const AValue : Boolean): Boolean                        ;
      function    ReadBit     (var   AValue : Boolean): Boolean                        ;
      procedure   WaitBus                                                              ;
      function    SelectAll                           : Boolean                        ;
      function    Select      (const ADevice : TOWROM   ): Boolean                  ;
      function    ReadROM     (var   ADevice : TOWROM   ): Boolean                  ;
      function    DevicesCount      : Integer                                          ;
      function    DevicesAlarmCount : Integer                                          ;
      property    Device     [index : Integer]: TOWROM read GetDevice;
      property    DeviceAlarm[index : Integer]: TOWROM read GetDeviceAlarm;

      procedure   SearchDevices;
      procedure   SearchDevicesAlarm;

      procedure   SearchClear                                                          ;
      property    BusyTime                           : Int64 read GetBusy write SetBusy;
  end                                                                                  ;


  { TOWDefDeviceice }

  TOWDefaultDevice = class(TObject)
    private
      fOneWire                          : TOneWire;
      fPOWER                            : Boolean;
      function    GetPower              : Boolean;
    protected
      fROM                              : TOWROM;
      fDATA                             : Pointer;
    public
      constructor Create   (const AOWire: TOneWire;
                            const  ADevice : TOWROM) ;
      destructor  Destroy               ; override ;
      property    TypeDevice            : Byte read fROM.dTYPE;
      function    TypeName              : String;
      function    LoadPower             : Boolean ;
      property    Bus                   : TOneWire read fOneWire;
      property    MAC                   : TMAC read fROM.dMAC;
      property    Power                 : Boolean read GetPower;
  end;



 const
   // КОМАНДЫ ONE-WIRE
   SKIP_ROM          = $CC; // Игнорировать МАС устройства
   MATCH_ROM         = $55; // Выбрать устройство по МАС
   READ_ROM          = $33; // Прочитать МАС устройства
   SEARCH_ROM        = $F0; // Искать МАС устройств
   ALARM_SEARCH      = $EC; // Искать МАС устройств с установленной тревогой

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

{ TOWDefDevice }

function    TOWDefaultDevice.GetPower                      : Boolean ;
begin
  Result:= fPOWER;
end;

constructor TOWDefaultDevice.Create           (const AOWire: TOneWire;
                                             const ADevice: TOWROM);
begin
  inherited Create;
  fROM     := ADevice;
  fOneWire := AOWire;
  fDATA    := nil;
  fPOWER   := false;
end;

destructor  TOWDefaultDevice.Destroy                                 ;
begin
  FreeMem(fDATA);
  inherited Destroy;
end;

function    TOWDefaultDevice.TypeName                      : String  ;
begin
  Result     := 'UNABLE';
  case TypeDevice of
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
  $10: if ((fROM.dMAC[5] * 256 + fROM.dMAC[4]) >= 8)
  then Result:='DS18S20'
  else Result:='DS1820' ;
  $12: Result:='DS2604' ;
  $1A: Result:='DS1963L';
  $21: Result:='DS1921' ;
  $22: Result:='DS1822' ;
  $28: Result:='DS18B20';
  $33: Result:='DS1961S';
  $41: Result:='DS1922L';
  $89: Result:='DS1982U';
  $8B: Result:='DS1985U';
  $8F: Result:='DS1986U';
  end;
end;

function    TOWDefaultDevice.LoadPower                     : Boolean ;
var
  res: Boolean = false;
begin
  Result := false;
  Bus.WaitBus; // Ожидаем шину
  if not(Bus.fBus.Write(READ_POWER_SUPPLY)) then Exit;
  if not(Bus.fBus.ReadBit(res)) then Exit;
  fPOWER := res;
  Result := true;
end;

{ TOneWire }

function    TOneWire.Reset                             : Boolean   ;
var res : Byte                                                     ;
begin
  Result:= false                                                   ;
  if not(fBus.Config(false)    ) then Exit                         ;
  res   := $F0                                                     ;
  if not(fBus.WriteReadBit(res)) then Exit                         ;
  if not(fBus.Config           ) then Exit                         ;
  fBUSY := GetTickCount64                                          ;
  Result:=    (res <> $F0)                                         ;
end                                                                ;

function    TOneWire.Write      (const AValue: Byte   ): Boolean   ;
begin
  WaitBus                                                          ; // Ожидаем шину
  Result:= fBus.Write(AValue)                                      ;
end;

function    TOneWire.Read       (var   AValue: Byte   ): Boolean   ;
begin
  WaitBus; // Ожидаем шину
  Result:= fBus.Read(AValue);
end;

function    TOneWire.WriteBit   (const AValue: Boolean): Boolean   ;
begin
  WaitBus; // Ожидаем шину
  Result:= fBus.WriteBit(AValue);
end;

function    TOneWire.ReadBit    (var   AValue: Boolean): Boolean   ;
begin
  WaitBus; // Ожидаем шину
  Result:= fBus.ReadBit(AValue);
end;

procedure   TOneWire.WaitBus                                       ;
begin
  while BusyTime > 0 do Sleep(10);
end;

function    TOneWire.GetBusy                           : Int64     ;
begin
  Result:= fBUSY - GetTickCount64;
  if Result < 0 then Result:= 0;
end;

function TOneWire.GetDevice(index : Integer): TOWROM;
begin
  Result:= fDEVICES[index];
end;

function TOneWire.GetDeviceAlarm(index : Integer): TOWROM;
begin
  Result:= fALARMS[index];
end;

function    TOneWire.SelectAll                         : Boolean   ;
begin
  Result:= false;
  WaitBus; // Ожидаем шину
  if not(Reset) then Exit;
  if not(fBus.Write(SKIP_ROM)) then Exit;
  Result:= true;
end;

function    TOneWire.Select     (const ADevice   : TOWROM): Boolean   ;
var ind : Integer;
begin
  Result:= false;
  WaitBus; // Ожидаем шину
  if not(Reset) then Exit;
  if not(fBus.Write(MATCH_ROM)) then Exit;
    for ind:= 0 to Pred(SizeOf(TOWROM)) do if not(fBus.Write(PByte(@ADevice)[ind])) then Exit;
  Result:= true;
end;

function    TOneWire.ReadROM    (var   ADevice   : TOWROM): Boolean   ;
var ind : Integer = 0;
begin
  Result:= false;
  WaitBus; // Ожидаем шину
  if not(Reset) then Exit;
  if not(fBus.Write(READ_ROM)) then Exit;
  for ind:= 0 to Pred(SizeOf(TOWROM)) do if not(fBus.Read(PByte(@ADevice)[ind])) then Exit; // Family
  Result:= true;
end;

function    TOneWire.SearchDevices     (var   ADevice   : TOWROM): Boolean   ;
begin
  Result:= false;
  WaitBus; // Ожидаем шину
  if not(Reset) then Exit;
  if not(fBus.Write(SEARCH_ROM)) then Exit;
  if not(SearchEx(ADevice)) then Exit;
  Result:= true;
end;

function    TOneWire.SearchDevicesAlarm(var   ADevice   : TOWROM): Boolean   ;
begin
  Result:= false;
  WaitBus; // Ожидаем шину
  if not(Reset                           ) then Exit;
  if not(fBus.Write(ALARM_SEARCH)) then Exit;
  if not(SearchEx   (ADevice                 )) then Exit;
  Result:= true;
end;

function TOneWire.DevicesCount: Integer;
begin
  Result:= Length(fDEVICES);
end;

function TOneWire.DevicesAlarmCount: Integer;
begin
  Result:= Length(fALARMS);
end;

procedure TOneWire.SearchDevices;
var
  aDevice : TOWROM;
begin
  while Self.SearchDevices(aDevice{%H-}) do
  begin
    SetLength(fDEVICES, Length(fDEVICES) + 1);
    fDEVICES[Length(fDEVICES) - 1]:= aDevice;
  end;
end;

procedure TOneWire.SearchDevicesAlarm;
var
  aDevice : TOWROM;
begin
  while Self.SearchDevicesAlarm(aDevice{%H-}) do
  begin
    SetLength(fALARMS, Length(fALARMS) + 1);
    fALARMS[Length(fALARMS) - 1]:= aDevice;
  end;
end;

procedure   TOneWire.SearchClear                                   ;
begin
  fEROM        := 0                ;
  fENDD        := false            ;
end;

function    TOneWire.SearchEx   (var   ADevice   : TOWROM): Boolean   ;
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
      if not(fBus.ReadBit(a_bit)) then Exit;                             // Читаем 1-ый бит
      if not(fBus.ReadBit(b_bit)) then Exit;                             // Читаем 2-ой бит
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
          if not(fBus.WriteBit(rom_search)) then Exit;                  //серийный номер направление поиска бит записи
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
      fEROM := last_zero;                                               // поиск успешен,поэтому установите LastDiscrepancy, LastDeviceiceFlag,search_result
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

procedure   TOneWire.SetBusy    (const AValue : Int64)             ;
begin
  if ((GetTickCount64 + AValue) > fBUSY)
  then fBUSY:= GetTickCount64 + AValue;
end;

constructor TOneWire.Create     (const ADevice: String)            ;
begin
  inherited Create                           ;
  fBUSY        := GetTickCount64             ;
  fEROM        := 0                          ;
  fENDD        := false                      ;
  fBUS         := TOWBUS.Create(ADevice)     ;

end;

destructor  TOneWire.Destroy                                       ;
begin
  fBus.Free;
  inherited Destroy;
end;

end.

