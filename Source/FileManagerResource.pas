unit FileManagerResource;

interface

uses
  uTranslate;

resourcestring

  cAskSave = 'Do you want to save changes?File: %s?';
  cNoFileKinds = 'No TCustomFiles component assigned to the File Manager';
  cOpenedMenuName = 'mMdOpenedFile';
  cSaveLoaderOutOfBounds = 'Save-loader index out of bounds: %d!';
  cEmpty = '   ';

  cErrDiskWrite = 'Cannot write to disk! Disk is full or write protected!';
  cErrDiskSpace = 'Cannot write to disk! Disk is full!';
  cFileExists = 'File with name ''%s'' already exists! Do you want to override it?';

  procedure addToTranslator();

implementation

procedure addToTranslator();
begin
  gTranslate.addToResourceList(Pointer(@FileManagerResource.cAskSave), 'FileManagerResource.cAskSave');
  gTranslate.addToResourceList(Pointer(@FileManagerResource.cErrDiskWrite), 'FileManagerResource.cErrDiskWrite');
  gTranslate.addToResourceList(Pointer(@FileManagerResource.cFileExists), 'FileManagerResource.cFileExists');
end;

end.
