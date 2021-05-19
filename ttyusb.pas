unit ttyUSB;

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

uses SysUtils, {$IFDEF Unix} BaseUnix, TermIO {$ENDIF} {$IFDEF Windows} Windows {$ENDIF}   ;

function  OWOpen        (const ADevice : String )                               : THandle  ;
function  OWConf        (const AHandle : THandle; const ASpeed : Boolean = true): Boolean  ;
procedure OWClose       (const AHandle : THandle)                                          ;
function  OWWriteReadBit(const AHandle : THandle; var   AValue : Byte   )       : Boolean  ;
function  OWReadBit     (const AHandle : THandle; var   AValue : Boolean)       : Boolean  ;
function  OWWriteBit    (const AHandle : THandle; const AValue : Boolean)       : Boolean  ;
function  OWWrite       (const AHandle : THandle; const AValue : Byte   )       : Boolean  ;
function  OWRead        (const AHandle : THandle; var   AValue : Byte   )       : Boolean  ;

const
  {$IFDEF Unix}
    OW_INVALID_HANDLE    = INVALID_HANDLE_VALUE                                            ;
  {$ENDIF}
  {$IFDEF Windows}
    OW_INVALID_HANDLE    = INVALID_HANDLE_VALUE                                            ;
  {$ENDIF}
var
  {$IFDEF Unix}
    old_term                                    : TermIOS                                  ;
  {$ENDIF}
  {$IFDEF Windows}
    old_term                                    : TDCB                                     ;
  {$ENDIF}

implementation

// Функция возвращает дескриптор устройства управления шиной "адин-дрот".
// В качестве параметра принимает название последовательного порта.
// Для Винды - рекомендуется формат '//./COM3', особенно если выше 9 ком-порта.
// Для Линя  - рекомендуемый формат '/dev/ttyUSB0' или какой там у вас адаптер.
function  OWOpen        (const ADevice : String                                ): THandle  ;
begin
  {$IFDEF Unix}
  Result      := FileOpen(ADevice, O_RDWR or O_NONBLOCK or O_NDELAY)                       ;
  {$ENDIF}
  {$IFDEF Windows}
  Result      :=  CreateFile(PChar(ADevice), GENERIC_READ or GENERIC_WRITE,
                             0, NIL, OPEN_EXISTING, 0, 0)                                  ;
  {$ENDIF}
  if (Result   = OW_INVALID_HANDLE)         then Exit                                      ;
  {$IFDEF Unix}
  if TCGetAttr(Result, old_term) = INVALID_HANDLE then Exit                                ;
  {$ENDIF}
  {$IFDEF Windows}
  if not(GetCommState(Result, old_term))    then Exit                                      ;
  {$ENDIF}
  if not(OWConf(Result))                    then Exit                                      ;
end;

// Функция управления устройством управления шиной "адин-дрот".
function  OWConf        (const AHandle : THandle; const ASpeed : Boolean = true): Boolean  ;
{$IFDEF Unix}
var
  term            :  TermIOS                                                               ;
  spd             :  Cardinal = B115200                                                    ;
begin
  Result          := false                                                                 ;
  if TCGetAttr(AHandle, term{%H-})        = INVALID_HANDLE then Exit                       ;
  CFMakeRaw   (term)                                                                       ;
  term.c_cc[VTIME]:= 1                                                                     ;
  term.c_cc[VMIN ]:= 1                                                                     ;
  if not(ASpeed) then spd:= B9600                                                          ;
  CFSetISpeed (term, spd)                                                                  ;
  CFSetOSpeed (term, spd)                                                                  ;
  if TCSetAttr(AHandle, TCSAFLUSH, term)  = INVALID_HANDLE then Exit                       ;
  if TCFlush  (AHandle, TCIOFLUSH      )  = INVALID_HANDLE then Exit                       ;
{$ENDIF}
{$IFDEF Windows}
var
  term            :  TDCB                                                                  ;
  spd             :  Cardinal  = CBR_115200                                                ;
begin
  Result          := false                                                                 ;
  if not(GetCommState(AHandle, term{%H-}))            then Exit                            ;
  if not(ASpeed)
    then spd      := CBR_9600                                                              ;
  term.BaudRate   := spd                                                                   ;
  term.Parity     := NOPARITY                                                              ;
  term.ByteSize   := 8                                                                     ;
  term.StopBits   := ONESTOPBIT                                                            ;
  if not(SetCommState(AHandle, term))                 then Exit                            ;
{$ENDIF}
  Result:= true                                                                            ;
end;

// Процедура закрывает дескриптор устройства управления шиной "адин-дрот".
procedure OWClose       (const AHandle : THandle                               )           ;
begin
  {$IFDEF Unix}
  TCSetAttr(AHandle, TCSAFLUSH, old_term)                                                  ;
  {$ENDIF}
  {$IFDEF Windows}
  SetCommState(AHandle, old_term)                                                          ;
  {$ENDIF}
  FileClose(AHandle)                                                                       ;
end;


// АПИ функция выполняет запись и чтение бита в шину "адин-дрот" возвращает истину если все удачно.
function  OWWriteReadBit(const AHandle : THandle; var  AValue  : Byte          ): Boolean  ;
begin
  Result          := false                                                                 ;
  if AHandle       = OW_INVALID_HANDLE                then Exit                            ;
  if FileWrite(AHandle,   AValue, SizeOf(AValue)) < 0 then Exit                            ;
  if FileRead (AHandle,   AValue, SizeOf(AValue)) < 0 then Exit                            ;
  Result          := true                                                                  ;
end                                                                                        ;

// АПИ функция выполняет запись бита в шину "адин-дрот" возвращает истину если все удачно.
function  OWWriteBit    (const AHandle : THandle; const AValue : Boolean       ): Boolean  ;
var
  res             :  Byte     = $FF                                                        ;
begin
  if not(AValue)
    then res      := $00                                                                   ;
  Result          := OWWriteReadBit(AHandle, res)                                          ;
end;

// АПИ функция выполняет чтение бита с шины "адин-дрот" возвращает истину если все удачно.
function  OWReadBit     (const AHandle : THandle; var  AValue  : Boolean       ): Boolean  ;
var
  res             :  Byte    = $FF                                                         ;
begin
  Result          := OWWriteReadBit(AHandle, res)                                          ;
  AValue          := res     = $FF                                                         ;
end;

// Функция выполняет запись и чтение байта в шину "адин-дрот" возвращает истину если все удачно.
// Базовая функция для функций чтения и записи в шину "адин-дрот".
function  OWWriteRead   (const AHandle : THandle; var   aValue : Byte          ): Boolean  ;
var
  ind             :  Integer = 0                                                           ;
  res             :  Byte    = $00                                                         ;
begin
  Result          := false                                                                 ;
  repeat
    inc(ind)                                                                               ;
    if Odd(avalue)
      then   res  := $FF
      else   res  := $00                                                                   ;
    OWWriteReadBit(AHandle, res)                                                           ;
    aValue        := aValue shr 1                                                          ;
    if Odd(res)
      then aValue := aValue or $80                                                         ;
  until ind > 7                                                                            ;
  Result          := true                                                                  ;
end                                                                                        ;

// АПИ функция выполняет запись байта в шину "адин-дрот" возвращает истину если все удачно.
function  OWWrite       (const AHandle : THandle; const aValue : Byte          ): Boolean  ;
var
  tmp             :  Byte    = 0                                                           ;
begin
  tmp             := aValue                                                                ;
  Result          := OWWriteRead(AHandle, tmp)                                             ;
end                                                                                        ;

// АПИ функция выполняет чтение байта с шины "адин-дрот" возвращает истину если все удачно.
function  OWRead        (const AHandle : THandle; var   aValue : Byte          ): Boolean  ;
var
  tmp             :  Byte    = $FF                                                         ;
begin
  Result          := OWWriteRead(AHandle, tmp)                                             ;
  aValue          := tmp                                                                   ;
end                                                                                        ;

end.
