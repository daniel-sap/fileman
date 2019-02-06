unit DocumentReader;

interface

uses
  SaveLoader, Classes;

type

  TDocumentReader = class(TSaveLoader)
  public
    function AcceptVisitor(aVisited: TObject): Boolean; override;
    procedure execute(aModel: TObject; Stream: TStream); override;
  end;

implementation

uses
  DocumentModel;

{ TDocumentReader }

function TDocumentReader.AcceptVisitor(aVisited: TObject): Boolean;
begin
  Result := aVisited is TDocumentModel;
end;

procedure TDocumentReader.execute(aModel: TObject; Stream: TStream);
var
  reader: TStreamReader;
begin
  reader := TStreamReader.Create(Stream);

  TDocumentModel(aModel).Text := reader.ReadToEnd;

  reader.Free;
end;

end.
