unit ttyUSB;

{$mode objfpc}{$H+}
//{$DEFINE DEBUG}
interface

uses SysUtils,
  {$IFDEF Unix}
    BaseUnix, TermIO
  {$ENDIF}
  {$IFDEF Windows}
    Windows
  {$ENDIF};


function  OpenTTY   (const ADevice : String ): THandle;
function  ConfTTY   (const AHandle : THandle; const ASpeed : Boolean = true): Boolean;
procedure CloseTTY  (const AHandle : THandle);

function  wr_bit    (const AHandle : THandle; var   AValue : Byte   ): Boolean;
function  r_bit     (const AHandle : THandle; var   AValue : Boolean): Boolean;
function  w_bit     (const AHandle : THandle; const AValue : Boolean): Boolean;

function  wr_byte   (const AHandle : THandle; var   AValue : Byte   ): Boolean;
function  WriteTTY  (const AHandle : THandle; const AValue : Byte   ): Boolean;
function  ReadTTY   (const AHandle : THandle; var   AValue : Byte   ): Boolean;

const
  {$IFDEF Unix}
    INVALID_HANDLE    = INVALID_HANDLE_VALUE;
  {$ENDIF}
  {$IFDEF Windows}
    INVALID_HANDLE    = INVALID_HANDLE_VALUE;
  {$ENDIF}
var
  {$IFDEF Unix}
    old_term                                    : TermIOS                         ;
  {$ENDIF}
  {$IFDEF Windows}
    old_term                                    : TDCB                            ;
  {$ENDIF}



implementation

function    OpenTTY    (const ADevice : String): THandle;
begin
  {$IFDEF Unix}
  Result      := FileOpen(ADevice, O_RDWR or O_NONBLOCK or O_NDELAY);
  {$ENDIF}
  {$IFDEF Windows}
  Result      :=  CreateFile(PChar(ADevice), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  {$ENDIF}
  if (Result   = INVALID_HANDLE)         then Exit;
  {$IFDEF Unix}
  if TCGetAttr(Result, old_term) = INVALID_HANDLE then Exit;
  {$ENDIF}
  {$IFDEF Windows}
  if not(GetCommState(Result, old_term)) then Exit;
  {$ENDIF}
  if not(ConfTTY(Result))                then Exit;
end;




{$IFDEF Unix}
function    ConfTTY    (const AHandle : THandle; const ASpeed : Boolean = true): Boolean;
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
{$ENDIF}
{$IFDEF Windows}
function    ConfTTY    (const AHandle : THandle; const ASpeed : Boolean = true): Boolean;
var
  term : TDCB;
  spd  : Cardinal = CBR_115200;
  TimeOuts : TCOMMTIMEOUTS;
begin
  Result:= false;
  if not(GetCommState(AHandle, term)) then Exit;
  if not(ASpeed) then spd:= CBR_9600;
  term.BaudRate := spd;
  term.Parity   := NOPARITY;
  term.ByteSize := 8;
  term.StopBits := ONESTOPBIT;
  //if not(GetCommTimeOuts(AHandle, TimeOuts)) then Exit;
  //if not(SetCommTimeOuts(AHandle, TimeOuts)) then Exit;
  if not(SetCommState(AHandle, term)) then Exit;
  Result:= true;
end;
{$ENDIF}



procedure   CloseTTY  (const AHandle : THandle);
begin
  {$IFDEF Unix}
  TCSetAttr(AHandle, TCSAFLUSH, old_term);
  {$ENDIF}
  {$IFDEF Windows}
  SetCommState(AHandle, old_term);
  {$ENDIF}
  FileClose(AHandle);
end;

function    wr_bit  (const AHandle : THandle; var  AValue  : Byte   ): Boolean   ;
begin
  Result       := false                                                             ;
  if AHandle    = INVALID_HANDLE then Exit                                          ;
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

