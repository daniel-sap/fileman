unit FileHolder;

interface

uses
  ObserverObjects;

const
  PROPERTY_MODIFIED = 'property_modified';
  PROPERTY_ISNEW = 'property_isnew';
  PROPERTY_FILE_NAME = 'property_file_name';
  
type

  TFileHolder = class(TSubjectObject)
  private
    fIsNew: boolean;
    fFileName: string;
    fModified: Boolean;
    fModel: TObject;
    fId: Integer;
    procedure setModified(const Value: Boolean);
    procedure setIsNew(const Value: boolean);
    procedure setFileName(const Value: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    // Name of the file where the model is saved
    property FileName: string read fFileName write setFileName;
    // True if model is new and is not saved yet
    property isNew: boolean read fIsNew write setIsNew;

    // True if the model differs from the saved on hard disk
    property Modified: Boolean read fModified write setModified;

    // The model that contains the data
    property Model: TObject read fModel write fModel;
    property Id: Integer read fId write fId;
  end;

implementation

constructor TFileHolder.Create;
begin
  inherited;
  fIsNew := True;
  fModified := False;
end;

destructor TFileHolder.Destroy;
begin
  Model.Free;
  inherited;
end;

procedure TFileHolder.setFileName(const Value: string);
begin
  if (Value <> fFileName) then begin
    fFileName := Value;
    notifyAll(PROPERTY_FILE_NAME, nil, nil);
  end;
end;

procedure TFileHolder.setIsNew(const Value: boolean);
begin
  if (Value <> fIsNew) then begin
    fIsNew := Value;
    notifyAll(PROPERTY_ISNEW, nil, nil);
  end;
end;

procedure TFileHolder.setModified(const Value: Boolean);
begin
  if (Value <> fModified) then begin
    fModified := Value;
    notifyAll(PROPERTY_MODIFIED, nil, nil);
  end;
end;

end.
