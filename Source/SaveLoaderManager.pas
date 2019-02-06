unit SaveLoaderManager;

interface

uses
  Classes, SaveLoader, Generics.Collections;

type

  TSaveLoaderManager = class
  private
    fReaders: TObjectList<TSaveLoader>;
    fWriters: TObjectList<TSaveLoader>;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure registerReader(aReader: TSaveLoader);
    procedure registerWriter(aWriter: TSaveLoader);
    function Save(aFile: TObject; aStream: TStream): Boolean;
    function Load(aFile: TObject; aStream: TStream): Boolean;
  end;

  function gSaveLoaderMgr: TSaveLoaderManager;
  
implementation

uses
  SysUtils, FileManagerResource;

var
  vSaveLoader: TSaveLoaderManager = nil;

function gSaveLoaderMgr: TSaveLoaderManager;
begin
  if vSaveLoader = nil then
    vSaveLoader := TSaveLoaderManager.Create;
  Result := vSaveLoader;
end;

constructor TSaveLoaderManager.Create;
begin
  inherited Create;
  fReaders := TObjectList<TSaveLoader>.Create();
  fWriters := TObjectList<TSaveLoader>.Create();
end;

destructor TSaveLoaderManager.Destroy;
begin
  FreeAndNil(fReaders);
  FreeAndNil(fWriters);
  inherited Destroy;
end;

procedure TSaveLoaderManager.registerReader(aReader: TSaveLoader);
begin
  fReaders.Add(aReader);
end;

procedure TSaveLoaderManager.registerWriter(aWriter: TSaveLoader);
begin
  fWriters.Add(aWriter);
end;

function TSaveLoaderManager.Save(aFile: TObject; aStream: TStream): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to fWriters.Count -1 do
    if fWriters[i].AcceptVisitor(aFile) then
      if fWriters[i].execute(aFile, aStream) then
        Result := True;
end;

function TSaveLoaderManager.Load(aFile: TObject; aStream: TStream): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to fReaders.Count -1 do begin
    if fReaders[i].AcceptVisitor(aFile) then begin
      Result := fReaders[i].execute(aFile, aStream);
      if Result then
        Exit;
    end;
  end;
end;

initialization

finalization
  vSaveLoader.Free;

end.
