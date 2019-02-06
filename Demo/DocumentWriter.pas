unit DocumentWriter;

interface

uses
  SaveLoader, Classes;

type

  TDocumentWriter = class(TSaveLoader)
  public
    function AcceptVisitor(aVisited: TObject): Boolean; override;
    procedure execute(aModel: TObject; Stream: TStream); override;
  end;

implementation

uses
  DocumentModel;

{ TDocumentWriter }

function TDocumentWriter.AcceptVisitor(aVisited: TObject): Boolean;
begin
  Result := aVisited is TDocumentModel;
end;

procedure TDocumentWriter.execute(aModel: TObject; Stream: TStream);
var
  writer: TStreamWriter;
begin
  writer := TStreamWriter.Create(Stream);
  writer.Write(TDocumentModel(aModel).Text);
  writer.Free;
end;

end.
