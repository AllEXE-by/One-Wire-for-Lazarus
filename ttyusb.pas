unit ttyUSB;

{$mode objfpc}{$H+}
//{$DEFINE DEBUG}
interface

uses
  SysUtils, BaseUnix, TermIO;

function  OpenTTY   (const ADevice : String ): THandle;
function  ConfTTY   (const AHandle : Integer; const ASpeed : Boolean = true): Boolean;
procedure CloseTTY  (const AHandle : THandle);

function  wr_bit    (const AHandle : THandle; var AValue : Byte): Boolean   ;
function  r_bit     (const AHandle : THandle; var AValue : Boolean): Boolean   ;
function  w_bit     (const AHandle : THandle; const AValue : Boolean): Boolean   ;

function  wr_byte   (const AHandle : THandle; var   aValue : Byte)   : Boolean;
function  WriteTTY  (const AHandle : THandle; const aValue : Byte)   : Boolean;
function  ReadTTY   (const AHandle : THandle; var   aValue : Byte)   : Boolean;

const
  INVALID_HANDLE    = THandle(-1);

var
  old_term                                    : TermIOS                         ;

implementation

function    OpenTTY    (const ADevice : String): THandle;
begin
  Result      := FileOpen(ADevice, O_RDWR or O_NONBLOCK or O_NDELAY);
  if (Result   = INVALID_HANDLE)                  then Exit;
  if TCGetAttr(Result, old_term) = INVALID_HANDLE then Exit;
  if not(ConfTTY(Result))                         then Exit;
end;

function    ConfTTY    (const AHandle : Integer; const ASpeed : Boolean = true): Boolean;
var
  term : TermIOS;
  spd  : Cardinal = B115200;
begin
  Result:= false;
  if TCGetAttr(AHandle, term{%H-})        = INVALID_HANDLE then Exit;
  CFMakeRaw   (term)                                                ;
  term.c_cc[VTIME]:= 1                                              ;
  term.c_cc[VMIN ]:= 1                                              ;
  if not(ASpeed) then spd:= B9600                                   ;
  CFSetISpeed (term, spd)                                           ;
  CFSetOSpeed (term, spd)                                           ;
  if TCSetAttr(AHandle, TCSAFLUSH, term)  = INVALID_HANDLE then Exit;
  if TCFlush  (AHandle, TCIOFLUSH      )  = INVALID_HANDLE then Exit;
  Result:= true;
end;

procedure   CloseTTY  (const AHandle : Integer);
begin
  TCSetAttr(AHandle, TCSAFLUSH, old_term);
  FileClose(AHandle);
end;

function    wr_bit  (const AHandle : THandle; var  AValue  : Byte   ): Boolean   ;
begin
  Result       := false                                                             ;
  if AHandle    = INVALID_HANDLE then Exit                                          ;                                               ;
  if FileWrite(AHandle,   AValue, SizeOf(AValue)) < 0 then Exit                ;
  if FileRead (AHandle,   AValue, SizeOf(AValue)) < 0 then Exit                ;
  Result       := true                                                              ;
end ;

function    r_bit   (const AHandle : THandle; var  AValue  : Boolean): Boolean   ;
var
  res : Byte = $FF;
begin
  Result:= wr_bit(AHandle, res);
  AValue:= res = $FF;
end;

function    w_bit   (const AHandle : THandle; const AValue : Boolean): Boolean;
var
  res : Byte = $FF;
begin
  if not(AValue) then res:= $00;
  Result:= wr_bit(AHandle, res);
end;

function    wr_byte (const AHandle : THandle; var   aValue : Byte   ): Boolean   ;
var
  ind          : Integer = 0                                                        ;
  res          : Byte    = $00                                                      ;
begin
  Result       := false                                                             ;
  repeat
    inc(ind)                                                                        ;
    if Odd(avalue) then   res:= $FF else res:= $00                                  ;
    wr_bit(AHandle, res);
    aValue     := aValue shr 1                                                      ;
    if Odd(res) then aValue  := aValue or $80                                       ;
  until ind > 7                                                                     ;
  Result       := true                                                              ;
end                                                                                 ;

function    WriteTTY(const AHandle : THandle; const aValue : Byte   ): Boolean   ;
var
  tmp          :  Byte   = 0                                                        ;
begin
  tmp          := aValue                                                            ;
  Result       := wr_byte(AHandle, tmp)                                             ;
end                                                                                 ;

function    ReadTTY (const AHandle : THandle; var   aValue : Byte   ): Boolean   ;
var
  tmp          : Byte    = $FF                                                      ;
begin
  Result       := wr_byte(AHandle, tmp)                                             ;
  aValue       := tmp                                                               ;
end                                                                                 ;


end.

