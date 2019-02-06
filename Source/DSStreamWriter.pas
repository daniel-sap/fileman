unit DSStreamWriter;

interface

uses
  Classes;

type

  TDSStreamWriter = class
  private
    fStream: TStream;
  public
    constructor create(aStream: TStream);
    destructor Destroy; override;
    procedure writeWord(Value: Word);
  end;

implementation

constructor TDSStreamWriter.create(aStream: TStream);
begin
  fStream := aStream;
end;

destructor TDSStreamWriter.destroy;
begin

  inherited;
end;

procedure TDSStreamWriter.writeWord(Value: Word);
begin
  fStream.WriteBuffer(Value, 2);
end;

end.
