unit OneWireBus;

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

{$mode objfpc}{$H+}{$DEFINE DEBUG}

interface

uses SysUtils, {$IFDEF Unix   } BaseUnix, TermIO {$ENDIF}
               {$IFDEF Windows} Windows          {$ENDIF};

type
{ TOWBus }

TOWBus = class(TObject)
  private
    fHandle                                : THandle                    ;
    fOldTerm                               : {$IFDEF Unix} TermIOS {$ENDIF}
                                             {$IFDEF Windows} TDCB {$ENDIF};
  public
    constructor Create       (const ADevice: String )                   ;
    destructor  Destroy                                     ; override  ;
    function    Config       (const ASpeed : Boolean = true): Boolean   ;
    function    WriteReadBit (var   AValue : Byte   )       : Boolean   ;
    function    WriteBit     (const AValue : Boolean)       : Boolean   ;
    function    ReadBit      (var   AValue : Boolean)       : Boolean   ;
    function    WriteRead    (var   AValue : Byte   )       : Boolean   ;
    function    Write        (const AValue : Byte   )       : Boolean   ;
    function    Read         (var   AValue : Byte   )       : Boolean   ;
end;

const
  {$IFDEF Unix}
    OW_INVALID_HANDLE    = INVALID_HANDLE_VALUE                         ;
  {$ENDIF}
  {$IFDEF Windows}
    OW_INVALID_HANDLE    = INVALID_HANDLE_VALUE                         ;
  {$ENDIF}

implementation

{ TOWBus }

constructor TOWBus.Create(const ADevice: String)                        ;
begin
  {$IFDEF Unix}
  fHandle     := FileOpen(ADevice, O_RDWR or O_NONBLOCK or O_NDELAY)    ;
  {$ENDIF}
  {$IFDEF Windows}
  fHandle     := CreateFile(PChar(ADevice), GENERIC_READ or GENERIC_WRITE,
                             0, NIL, OPEN_EXISTING, 0, 0)               ;
  {$ENDIF}
  if (fHandle  = OW_INVALID_HANDLE)         then Exit                   ;
  {$IFDEF Unix}
  if TCGetAttr(fHandle, fOldTerm) = INVALID_HANDLE then Exit            ;
  {$ENDIF}
  {$IFDEF Windows}
  if not(GetCommState(fHandle, fOldTerm))   then Exit                   ;
  {$ENDIF}
  if not(Config)                            then Exit                   ;
end;

destructor TOWBus.Destroy;
begin
  {$IFDEF Unix}
  TCSetAttr(fHandle, TCSAFLUSH, fOldTerm)                               ;
  {$ENDIF}
  {$IFDEF Windows}
  SetCommState(fHandle, fOldTerm)                                       ;
  {$ENDIF}
  FileClose(fHandle)                                                    ;
  inherited Destroy;
end;

function TOWBus.Config(const ASpeed: Boolean): Boolean                  ;
  {$IFDEF Unix}
var
  term            :  TermIOS                                            ;
  spd             :  Cardinal = B115200                                 ;
begin
  Result          := false                                              ;
  if TCGetAttr(fHandle, term{%H-})        = INVALID_HANDLE then Exit    ;
  CFMakeRaw   (term)                                                    ;
  term.c_cc[VTIME]:= 1                                                  ;
  term.c_cc[VMIN ]:= 1                                                  ;
  if not(ASpeed) then spd:= B9600                                       ;
  CFSetISpeed (term, spd)                                               ;
  CFSetOSpeed (term, spd)                                               ;
  if TCSetAttr(fHandle, TCSAFLUSH, term)  = INVALID_HANDLE then Exit    ;
  if TCFlush  (fHandle, TCIOFLUSH      )  = INVALID_HANDLE then Exit    ;
  {$ENDIF}
  {$IFDEF Windows}
var
  term            :  TDCB                                               ;
  spd             :  Cardinal  = CBR_115200                             ;
begin
  Result          := false                                              ;
  if not(GetCommState(fHandle, term{%H-}))            then Exit         ;
  if not(ASpeed)
    then spd      := CBR_9600                                           ;
  term.BaudRate   := spd                                                ;
  term.Parity     := NOPARITY                                           ;
  term.ByteSize   := 8                                                  ;
  term.StopBits   := ONESTOPBIT                                         ;
  if not(SetCommState(fHandle, term))                 then Exit         ;
  {$ENDIF}
  Result:= true
end;

function TOWBus.WriteReadBit(var AValue: Byte): Boolean                 ;
begin
  Result          := false                                              ;
  if fHandle       = OW_INVALID_HANDLE                then Exit         ;
  if FileWrite(fHandle,   AValue, SizeOf(AValue)) < 0 then Exit         ;
  if FileRead (fHandle,   AValue, SizeOf(AValue)) < 0 then Exit         ;
  Result          := true                                               ;
end;

function TOWBus.WriteBit(const AValue: Boolean): Boolean                ;
var
  res             :  Byte     = $FF                                     ;
begin
  if not(AValue)
    then res      := $00                                                ;
  Result          := WriteReadBit(res)                                  ;
end;

function TOWBus.ReadBit(var AValue: Boolean): Boolean                   ;
var
  res             :  Byte    = $FF                                      ;
begin
  Result          := WriteReadBit(res)                                  ;
  AValue          := res     = $FF                                      ;
end;

function TOWBus.WriteRead(var AValue: Byte): Boolean                    ;
var
  ind             :  Integer = 0                                        ;
  res             :  Byte    = $00                                      ;
begin
  Result          := false                                              ;
  repeat
    inc(ind)                                                            ;
    if Odd(AValue)
      then   res  := $FF
      else   res  := $00                                                ;
    WriteReadBit(res)                                                   ;
    AValue        := AValue shr 1                                       ;
    if Odd(res)
      then AValue := AValue or $80                                      ;
  until ind > 7                                                         ;
  Result          := true                                               ;
end;

function TOWBus.Write(const AValue: Byte): Boolean                      ;
var
  tmp             :  Byte    = 0                                        ;
begin
  tmp             := aValue                                             ;
  Result          := WriteRead(tmp)                                     ;
end;

function TOWBus.Read(var AValue: Byte): Boolean                         ;
var
  tmp             :  Byte    = $FF                                      ;
begin
  Result          := WriteRead(tmp)                                     ;
  AValue          := tmp                                                ;
end                                                                     ;

end.

