unit FileManagerResource;

interface

uses
  Classes;

resourcestring

  cAskSave = 'Do you want to save changes?File: %s?';
  cNoFileKinds = 'No TCustomFiles component assigned to the File Manager';
  cOpenedMenuName = 'mMdOpenedFile';
  cSaveLoaderOutOfBounds = 'Save-loader index out of bounds: %d!';
  cEmpty = '   ';

  cErrDiskWrite = 'Cannot write to disk! Disk is full or write protected!';
  cErrDiskSpace = 'Cannot write to disk! Disk is full!';
  cFileExists = 'File with name ''%s'' already exists! Do you want to override it?';

  function getResources(): TStringList;

implementation

function getResources(): TStringList;
begin
  Result := TStringList.Create();
  Result.AddObject('FileManagerResource.cAskSave', Pointer(@FileManagerResource.cAskSave));
  Result.AddObject('FileManagerResource.cErrDiskWrite', Pointer(@FileManagerResource.cErrDiskWrite));
  Result.AddObject('FileManagerResource.cFileExists', Pointer(@FileManagerResource.cFileExists));
end;

end.
