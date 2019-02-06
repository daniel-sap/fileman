unit CustomFiles;

interface

type

  TMdCustomFiles = class
  public
    function CreateModel: TObject; virtual;

    function FileExt(out Description: string): string; virtual;
    function isFromClass(aModel: TObject): Boolean; virtual;
  end;

implementation

function TMdCustomFiles.CreateModel: TObject;
begin
  Result := nil;
end;

function TMdCustomFiles.FileExt(out Description: string): string;
begin
  // override in descendant
end;

function TMdCustomFiles.isFromClass(aModel: TObject): Boolean;
begin
  Result := False;
end;

end.
