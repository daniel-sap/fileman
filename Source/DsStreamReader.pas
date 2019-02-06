unit DsStreamReader;

interface

uses
  Classes;

type

  TDsStreamReader = class
  private
    fStream: TStream;
  public
    constructor create(aStream: TStream);
    destructor Destroy; override;
    function readWord: Word;
    function ReadInteger: Integer;
    function ReadInt64: Int64;
    function ReadFloat: Double;
    function ReadBool: Boolean;
    function ReadString: string;
    function ReadAnsiString: AnsiString;
    function readChar: Char;
    function readAnsiChar: AnsiChar;
    function readAnsiChars(count: Integer): AnsiString;

    function readEncInt: Integer;

  end;

implementation

uses
  SysUtils;

constructor TDsStreamReader.create(aStream: TStream);
begin
  fStream := aStream;
end;

destructor TDsStreamReader.destroy;
begin

  inherited;
end;

function TDsStreamReader.readWord: Word;
begin
  fStream.ReadBuffer(Result, 2);
end;

function TDsStreamReader.ReadInteger: Integer;
begin
  fStream.Read(Result, SizeOf(Integer));
end;

function TDsStreamReader.ReadInt64: Int64;
begin
  fStream.Read(Result, SizeOf(Int64));
end;

function TDsStreamReader.ReadFloat: Double;
begin
  fStream.Read(Result, SizeOf(Double));
end;

function TDsStreamReader.ReadString: string;
var
  vChar: Char;
begin
  Result := EmptyStr;
  fStream.Read(vChar, 2);
  while vChar <> #0 do begin
    Result := Result + vChar;
    fStream.Read(vChar, 2);
  end;
end;

function TDsStreamReader.ReadAnsiString: AnsiString;
var
  vChar: AnsiChar;
begin
  Result := EmptyAnsiStr;
  fStream.Read(vChar, 1);
  while vChar <> #0 do begin
    Result := Result + vChar;
    fStream.Read(vChar, 1);
  end;
end;

function TDsStreamReader.ReadBool: Boolean;
var
  vVal: Integer;
begin
  fStream.Read(vVal, SizeOf(Integer));
  Result := vVal > 0;
end;

function TDsStreamReader.readChar: Char;
begin
  fStream.Read(Result, 2);
end;

function TDsStreamReader.readEncInt: Integer;
var
  value: WordRec;
  temp: Byte;
begin
  fStream.Read(value, 2);
  temp := value.Hi;
  value.Hi := value.Lo;
  value.Lo := temp;
  Result := Word(value);
end;

function TDsStreamReader.readAnsiChar: AnsiChar;
begin
  fStream.Read(Result, 1);
end;

function TDsStreamReader.readAnsiChars(count: Integer): AnsiString;
begin
  SetLength(Result, count);
  fStream.Read(Result[1], count);
end;

end.
