unit DSFileManager;

interface

uses
  Classes, Menus, Contnrs, ExtCtrls, SaveLoaderManager, CustomFiles, FileHolder,
  SaveLoader, Generics.Collections, ObserverIntf;

const
  { Ini Files }
  INISEC_RECENT = 'RECENT_FILES';
  INIDENT_COUNT = 'Count';
  INIDENT_ITEM = 'Item';

type

  TFileEvent = procedure (Sender: TObject; Code: Word) of object;
  TFileCloseEvent = procedure (Sender: TObject; var CloseAction: Integer) of object;
  TFileHolderEvent = procedure (Sender: TObject; FileHolder: TFileHolder) of object;
  TCheckStringEvent = procedure (Sender: TObject; aText: string; var Allow: Boolean) of object;

  TDSFileManager = class(TComponent, IObserver)
  private
    fOpenedFiles: TObjectList;
    fActiveFile: TFileHolder;

    fFileKinds: TObjectList<TMdCustomFiles>;
    fNewFileName: string;
    fNewFileIndex: Integer;
    fRecent: TStrings;
    fIniFileName: string;
    fMaxRecent: Integer;
    fMenuReopen: TMenuItem;
    fMenuOpened: TMenuItem;

    fAutoSave: Boolean;
    fAutoSaveInterval: Integer;
    fAutoSaveTimer: TTimer;

    fOpenIndex: Integer;

    { Events }
    fOnNewFile: TNotifyEvent;
    fOnOpenFile: TNotifyEvent;
    fOnBeforeFileChange: TNotifyEvent;
    fOnFileChanged: TNotifyEvent;
    fOnFileClose: TFileCloseEvent;
    fOnBeforeSave: TFileHolderEvent;
    fOnAfterFileClose: TFileHolderEvent;
    fOnCheckFileOpen: TCheckStringEvent;
    fOnModifiedChanged: TNotifyEvent;

    function getActiveFile: TFileHolder;
    procedure setActiveFile(const Value: TFileHolder);
    function getIsOpened: Boolean;
    function getIsModified: Boolean;
    function getFile(Index: Integer): TFileHolder;
    function getFileCount: Integer;
    procedure setMaxRecent(Value: Integer);
    procedure setIniFileName(Value: string);
    procedure setMenuReopen(Value: TMenuItem);
    procedure setMenuOpened(Value: TMenuItem);
    function createFile(aModel: TObject; FileName: string; aIsNew: boolean): TFileHolder;
    function SaveToFile(aFileHolder: TFileHolder): Boolean;
    function getActiveModel: TObject;
    function extensionExists(aExt: String): Boolean;
  protected
    procedure doOpen(aFiles: TMdCustomFiles; FileName: string);
    procedure FileChanged; virtual;
    procedure AppendRecent(FileName: string);
    procedure RemoveRecent(RecentText: string);
    procedure MenuOpenedClick(Sender: TObject);
    procedure MenuReopenClick(Sender: TObject);
    procedure doBeforeFileChange;
    procedure doFileChanged;
    procedure setAutoSave(const Value: Boolean);
    procedure setAutoSaveInterval(const Value: Integer);
    function getKindForFile(aModel: TObject): TMdCustomFiles;
    procedure doModifiedChanged();
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AppendFiles(aFiles: TMdCustomFiles);
    procedure RemoveFiles(aFiles: TMdCustomFiles);

    procedure UpdateMenuReopen;
    procedure UpdateMenuOpened();
    //
    procedure New; overload;
    procedure New(aModel: TObject); overload;
    procedure Open; overload;
    procedure Open(FileName: string); overload;
    //
    procedure Save(); overload;
    procedure Save(aFileHolder: TFileHolder); overload;
    procedure SaveAs(); overload;
    procedure SaveAs(aFile: TFileHolder); overload;
    procedure SaveAll();
    //
    function Close: boolean;
    function CloseAll: boolean;

    procedure newExecute(Sender: TObject);
    procedure openExecute(Sender: TObject);
    procedure saveExecute(Sender: TObject);
    procedure saveAsExecute(Sender: TObject);
    procedure saveAllExecute(Sender: TObject);
    procedure closeExecute(Sender: TObject);
    procedure closeAllExecute(Sender: TObject);

    //
    procedure Reopen(RecentIdx: integer);
    //
    function getOpenFilter: string;
    function getSaveFilter: string;
    procedure SaveRecentToIni;
    procedure LoadRecentFromIni(aOnlyExisting: Boolean);
    procedure OpenedMenuUpdateChecked;

    procedure AutoSaveOnTimer(Sender: TObject);
    procedure AutoSaveFile(aFile: TFileHolder; aFileName: string);

    procedure registerReader(aReader: TSaveLoader);
    procedure registerWriter(aWriter: TSaveLoader);

    procedure Notify(aChange: TSubjectChangeData);

    property Files[Index: Integer]: TFileHolder read getFile; default;
    property FileCount: Integer read getFileCount;
    property ActiveFile: TFileHolder read getActiveFile write setActiveFile;
    property ActiveModel: TObject read getActiveModel;
    property AutoSave: Boolean read fAutoSave write SetAutoSave;
    property AutoSaveInterval: Integer read fAutoSaveInterval write setAutoSaveInterval;
  published
    property isOpened: Boolean read getIsOpened;
    property isModified: Boolean read getIsModified;
    property NewFileName: string read fNewFileName write fNewFileName;
    property IniFileName: string read fIniFileName write SetIniFileName;
    property MaxRecent: Integer read fMaxRecent write SetMaxRecent default 10;
    property Recent: TStrings read fRecent;
    property MenuReopen: TMenuItem read fMenuReopen write SetMenuReopen;
    property MenuOpened: TMenuItem read fMenuOpened write SetMenuOpened;

    property onNewFile: TNotifyEvent read fOnNewFile write fOnNewFile;
    property onOpenFile: TNotifyEvent read fOnOpenFile write fOnOpenFile;
    property OnBeforeFileChange: TNotifyEvent read fOnBeforeFileChange write fOnBeforeFileChange;
    property OnFileChanged: TNotifyEvent read fOnFileChanged write fOnFileChanged;
    property OnFileClose: TFileCloseEvent read fOnFileClose write fOnFileClose;
    property onAfterFileClose: TFileHolderEvent read fOnAfterFileClose write fOnAfterFileClose;
    property onBeforeSave: TFileHolderEvent read fOnBeforeSave write fOnBeforeSave;
    property onCheckFileOpen: TCheckStringEvent read fOnCheckFileOpen write fOnCheckFileOpen;
    property onModifiedChanged: TNotifyEvent read fOnModifiedChanged write fOnModifiedChanged;
  end;

  function MakeShortFileName(FileName: string; Len: Integer): string;

implementation

uses
  SysUtils, Controls, Forms, IniFiles, Dialogs, Math, FileManagerResource,
  RecentReader;

function MakeShortFileName(FileName: string; Len: Integer): string;
begin
  Result := ExtractFileName(FileName); // TO DO
end;

constructor TDSFileManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fOpenedFiles := TObjectList.Create;
  fActiveFile := nil;
  fFileKinds := TObjectList<TMdCustomFiles>.Create();
  fMenuOpened := nil;
  fMenuReopen := nil;
  fNewFileIndex := 1;
  fOpenIndex := 0;
  fMaxRecent := 10;
  fRecent := TStringList.Create;
  fAutoSave := True;
  fAutoSaveInterval := 5 * 60 * 1000; // 5 minutes
  fAutoSaveTimer := TTimer.Create(Self);
  fAutoSaveTimer.Interval := fAutoSaveInterval;
  fAutoSaveTimer.OnTimer := AutoSaveOnTimer;
  fAutoSaveTimer.Enabled := True;
end;

destructor TDSFileManager.Destroy;
begin
  if (fMenuOpened <> nil) then
    fMenuOpened.Clear;
  if (fMenuReopen <> nil) then
    fMenuReopen.Clear;
  while fFileKinds.Count > 0 do
    RemoveFiles(fFileKinds.Last);
  FreeAndNil(fFileKinds);
  FreeAndNil(fRecent);
  FreeAndNil(fOpenedFiles);
  inherited Destroy;
end;

procedure TDSFileManager.AppendFiles(aFiles: TMdCustomFiles);
begin
  fFileKinds.Add(aFiles);
end;

procedure TDSFileManager.RemoveFiles(aFiles: TMdCustomFiles);
begin
  fFileKinds.Remove(aFiles);
end;

function TDSFileManager.getIsOpened: Boolean;
begin
  Result := ActiveFile <> nil;
end;

function TDSFileManager.getIsModified: Boolean;
begin
  Result := False;
  if (ActiveFile = nil) then
    Exit;
  Result := ActiveFile.Modified;
end;

function TDSFileManager.getFile(Index: Integer): TFileHolder;
begin
  Result := TFileHolder(fOpenedFiles[Index]);
end;

function TDSFileManager.getFileCount: Integer;
begin
  Result := fOpenedFiles.Count;
end;

function TDSFileManager.getOpenFilter: string;
var
  i: Integer;
  Extention, Description: string;
begin
  if fFileKinds.Count > 0 then
  begin
    for i := 0 to fFileKinds.Count - 1 do begin
      Extention := fFileKinds.Items[i].FileExt(Description);
      Result := Result + Description + '|*.' + Extention + '|';
    end;
  end;
  Result := Result + 'All files (*.*)|*.*|';
end;

function TDSFileManager.getSaveFilter: string;
var
  i: Integer;
  Extention, Description: string;
begin
  if fFileKinds.Count > 0 then begin
    for i := 0 to fFileKinds.Count - 1 do begin
      Extention := fFileKinds.Items[i].FileExt(Description);
      Result := Result + Description + '|*.' + Extention + '|';
    end;
  end;
end;

function TDSFileManager.extensionExists(aExt: String): Boolean;
var
  i: Integer;
  Extension, Description: string;
begin
  Result := False;
  if fFileKinds.Count = 0 then
    Exit;
  if aExt[1] = '.' then
    aExt := aExt.Substring(1);
  aExt := aExt.toLower();

  for i := 0 to fFileKinds.Count - 1 do begin
    Extension := fFileKinds.Items[i].FileExt(Description);
    if (Extension.toLower() = aExt) then begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TDSFileManager.setMaxRecent(Value: Integer);
begin
  if Value <> fMaxRecent then begin
    fMaxRecent := Value;
  end;
end;

procedure TDSFileManager.SetIniFileName(Value: string);
begin
  if Value <> fIniFileName then begin
    fIniFileName := Value;
  end;
end;

procedure TDSFileManager.FileChanged;
begin
  OpenedMenuUpdateChecked;
  DoFileChanged;
  if (fActiveFile <> nil) then begin
    fActiveFile.subscribe(Self);
  end;
  doModifiedChanged();
end;

procedure TDSFileManager.doModifiedChanged;
begin
  if Assigned(fOnModifiedChanged) then begin
    fOnModifiedChanged(Self);
  end;
end;

procedure TDSFileManager.doBeforeFileChange;
begin
  if fActiveFile <> nil then begin
    fActiveFile.unSubscribe(Self);
  end;

  if Assigned(fOnBeforeFileChange) then
    fOnBeforeFileChange(Self);
end;

procedure TDSFileManager.DoFileChanged;
begin
  if Assigned(fOnFileChanged) then
    fOnFileChanged(Self);
end;

procedure TDSFileManager.AppendRecent(FileName: string);
var
  I, Idx: Integer;
begin
  // ��� ���������� ���� � ������� �� �������� �� ���
  for I := 0 to fRecent.Count - 1 do
    begin
      Idx := fRecent.IndexOf(FileName);
      if Idx >= 0 then
        fRecent.Delete(Idx)
      else
        Break;
    end;
  // ������ �� � �������� �� �������
  fRecent.Insert(0, FileName);
  // ������ ������� � ��-����� �� ���������� ���������� �� ������� ��������� ����,
  // �� �� �� ����� ��-����� �� ������� � ���.
  if (fRecent.Count > MaxRecent) and (MaxRecent > 0) then
    fRecent.Delete(fRecent.Count - 1);
  // ��������� �� ������� ����, �� ���������� ������������ ��������
  UpdateMenuReopen;
end;

procedure TDSFileManager.RemoveRecent(RecentText: string);
var
  Idx: Integer;
begin
  Idx := fRecent.IndexOf(RecentText);
  if Idx > -1 then begin
    fRecent.Delete(Idx);
    UpdateMenuReopen;
  end;
end;

procedure TDSFileManager.Reopen(RecentIdx: Integer);
begin
  if (RecentIdx >= 0) and (RecentIdx < Recent.Count) then begin
    DoOpen(fFileKinds.Items[0], Recent[RecentIdx]);
  end;
end;

procedure TDSFileManager.newExecute(Sender: TObject);
begin
  new();
end;

procedure TDSFileManager.Notify(aChange: TSubjectChangeData);
begin
  if (aChange.PropertyName = PROPERTY_MODIFIED) then begin
    doModifiedChanged();
  end;
end;

// Creates new file
procedure TDSFileManager.New;
var
  vFileKind: TMdCustomFiles;
begin
  if (fFileKinds.Count = 0) then
    Exit;
  vFileKind := fFileKinds.Items[0];
  New(vFileKind.CreateModel());
end;

// Creates new file
procedure TDSFileManager.New(aModel: TObject);
var
  fileHolder: TFileHolder;
  vFileName, Just: string;
  vFileKind: TMdCustomFiles;
begin
  vFileKind := getKindForFile(aModel);

  vFileName := NewFileName + IntToStr(fNewFileIndex) + '.' + vFileKind.FileExt(Just);
  
  fileHolder := createFile(aModel, vFileName, True);
  if (fileHolder = nil) then
    Exit;

  fileHolder.isNew := True;
  inc(fOpenIndex);
  fileHolder.Id := fOpenIndex;

  fOpenedFiles.Add(fileHolder);
  Inc(fNewFileIndex);
  UpdateMenuOpened();
  RemoveRecent(vFileName);

  if Assigned(fOnNewFile) then
    fOnNewFile(fileHolder);
  ActiveFile := fileHolder;
end;

// ������ ��� ����.
//  ����� ������������ �� ��� TCustomFiles, ����� �� ��������.
//  ��������� ����� ��� ���� ���� ����������� �� ���� �������.
//  ������ ����.
procedure TDSFileManager.Open(FileName: string);
begin
  if fFileKinds.Count > 0 then
    DoOpen(fFileKinds.Items[0], FileName);
end;

procedure TDSFileManager.openExecute(Sender: TObject);
begin
  open;
end;

// Shows open dialog and opens a file
procedure TDSFileManager.Open;
var
  openFileDialog: TOpenDialog;
begin
  if (fFileKinds.Count = 0) then begin
    ShowMessage(cNoFileKinds);
    exit;
  end;

  openFileDialog := TOpenDialog.Create(Self);
  try
    openFileDialog.Filter := GetOpenFilter;
    if openFileDialog.Execute then
      doOpen(fFileKinds.Items[0], openFileDialog.FileName);
  finally
    openFileDialog.Free;
  end;
end;

// Opens file of the given kind
procedure TDSFileManager.doOpen(aFiles: TMdCustomFiles; FileName: string);
var
  fileHolder: TFileHolder;
  vAllowOpen: Boolean;
begin
  vAllowOpen := True;
  if Assigned(fOnCheckFileOpen) then
    fOnCheckFileOpen(Self, FileName, vAllowOpen);

  if (not vAllowOpen) then
    exit;

  fileHolder := CreateFile(aFiles.CreateModel, FileName, False);
  if (fileHolder = nil) then
    Exit;


  fileHolder.isNew := False;
  inc(fOpenIndex);
  fileHolder.Id := fOpenIndex;

  fOpenedFiles.Add(fileHolder);
  UpdateMenuOpened();
  RemoveRecent(FileName);
  if Assigned(fOnOpenFile) then
    fOnOpenFile(fileHolder);
  ActiveFile := fileHolder;
end;

// Save the active file. Event to attach
procedure TDSFileManager.saveExecute(Sender: TObject);
begin
  Save();
end;

// Save the active file
procedure TDSFileManager.Save;
begin
  Save(ActiveFile);
end;

// ������� ��������� � ���������� ����
// ��� ������ � ��� �� �� ������� ��� ����� ���,
procedure TDSFileManager.Save(aFileHolder: TFileHolder);
begin
  // TODO - this check here must be outside the method
  if aFileHolder = nil then
    Exit;

  if aFileHolder.isNew then
    SaveAs(aFileHolder)
  else begin
    if SaveToFile(aFileHolder) then begin
      aFileHolder.Modified := False;
      UpdateMenuOpened();
    end;
  end;
end;

procedure TDSFileManager.saveAsExecute(Sender: TObject);
begin
  SaveAs();
end;

procedure TDSFileManager.SaveAs();
begin
  SaveAs(ActiveFile);
end;

procedure TDSFileManager.SaveAs(aFile: TFileHolder);
var
  sdlgMdSaveDialog: TSaveDialog;
  Just: string;
  vKind: TMdCustomFiles;
  fileExt: String;
  fileName: String;
begin
  if (aFile = nil) then
    Exit;
  vKind := getKindForFile(aFile.Model);
  sdlgMdSaveDialog := TSaveDialog.Create(Application);
  try
    sdlgMdSaveDialog.Filter := GetSaveFilter;
    sdlgMdSaveDialog.DefaultExt := vKind.FileExt(Just);
    sdlgMdSaveDialog.FileName := aFile.FileName;
    if sdlgMdSaveDialog.Execute then
      fileName := sdlgMdSaveDialog.FileName
    else
      Exit;
  finally
    sdlgMdSaveDialog.Free;
  end;

  if (not ActiveFile.isNew) then
    if (fileName <> ActiveFile.FileName)  then
      AppendRecent(ActiveFile.FileName);

  aFile.FileName := fileName;
  aFile.isNew := True;
  fileExt := ExtractFileExt(aFile.FileName);
  if not extensionExists(fileExt) then begin
    aFile.FileName := aFile.FileName + '.' + vKind.FileExt(Just);
  end;
  if FileExists(aFile.FileName) then begin
    if MessageDlg(Format(cFileExists, [aFile.FileName]), mtInformation, [mbOk, mbCancel], 0) <> mrOk then
      Exit;
  end;
  if SaveToFile(aFile) then begin
    aFile.isNew := False;
    aFile.Modified := False;
    UpdateMenuOpened();
  end;
end;

procedure TDSFileManager.saveAllExecute(Sender: TObject);
begin
  saveAll;
end;

// Save of all open files
procedure TDSFileManager.saveAll;
var
  i: Integer;
begin
  for i := 0 to fOpenedFiles.Count - 1 do
    Save(TFileHolder(fOpenedFiles[i]))
end;

procedure TDSFileManager.closeExecute(Sender: TObject);
begin
  close();
end;

function TDSFileManager.Close: Boolean;
var
  vActFile: TObject;

  function ModifiedDlg: Integer;
  var
    FileName: string;
    CloseAction: Integer;
  begin
    CloseAction := mrYes;
    FileName := ActiveFile.FileName;
    if Assigned(fOnFileClose) then
      fOnFileClose(Self, CloseAction)
    else begin
      CloseAction := MessageDlg(Format(cAskSave, [ActiveFile.FileName]), mtConfirmation, mbYesNoCancel, 0);
    end;
    Result := CloseAction;
  end;

begin
  Result := False;
  if ActiveFile = nil then
    Exit;

  if IsModified then
    case ModifiedDlg of
    mrYes:
      Save;
    mrNo: ;
    else
      Exit;
    end;

  if (not ActiveFile.isNew) then
    AppendRecent(ActiveFile.FileName);

  vActFile := fOpenedFiles.Extract(ActiveFile);
  if fOpenedFiles.Count > 0 then
    ActiveFile := TFileHolder(fOpenedFiles[0])
  else
    ActiveFile := nil;
  UpdateMenuOpened();

  if Assigned(fOnAfterFileClose) then
    fOnAfterFileClose(Self, TFileHolder(vActFile));

  vActFile.Free;
  Result := True;
end;

procedure TDSFileManager.closeAllExecute(Sender: TObject);
begin
  closeAll;
end;

// CloseAll
// ������ �� �� ������� ������ �������. ��� ������� ������ ���������� � True �
// �������� ������ ����� False.
function TDSFileManager.CloseAll: Boolean;
var
  Cnt: Integer;
begin
  Result := True;
  Cnt := 0;
  while FileCount > 0 do begin
    if not Close then begin
      Result := False;
      Break;
    end;
    Inc(Cnt);
  end;
  if Cnt > 0 then
    UpdateMenuOpened();
end;

procedure TDSFileManager.setMenuOpened(Value: TMenuItem);
begin
  if Value <> fMenuOpened then begin
    if fMenuOpened <> nil then
      fMenuOpened.Clear;
    fMenuOpened := Value;
    UpdateMenuOpened();
  end;
end;

procedure TDSFileManager.setMenuReopen(Value: TMenuItem);
begin
  if Value <> fMenuReopen then begin
    if fMenuReopen <> nil then
      fMenuReopen.Clear;
    fMenuReopen := Value;
    UpdateMenuReopen;
  end;
end;

procedure TDSFileManager.UpdateMenuOpened();
var
  i: Integer;
  NewItem: TMenuItem;
begin
  if fMenuOpened = nil then
    Exit;
  if csDesigning in ComponentState then
    Exit;
  fMenuOpened.Clear;

  if FileCount > 0 then begin
    for i := 0 to fOpenedFiles.Count - 1 do begin
      NewItem := TMenuItem.Create(Self);
      NewItem.Caption := TFileHolder(fOpenedFiles[i]).FileName;
      NewItem.Name := cOpenedMenuName + IntToStr(i);
      NewItem.Tag := i;
      NewItem.OnClick := MenuOpenedClick;
      FMenuOpened.Add(NewItem);
    end;
    fMenuOpened.Enabled := True;
    OpenedMenuUpdateChecked;
  end
  else begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := cEmpty;
    NewItem.Name := cOpenedMenuName + '0';
    NewItem.Enabled := False;
    fMenuOpened.Add(NewItem);
  end;
end;

procedure TDSFileManager.UpdateMenuReopen;
var
  i: Integer;
  NewItem: TMenuItem;
  Cnt: Integer;
begin
  if fMenuReopen = nil then
    Exit;
  if csDesigning in ComponentState then
    Exit;
  fMenuReopen.Clear;
  if fRecent.Count > 0 then begin
    if fRecent.Count > MaxRecent then
      Cnt := MaxRecent
    else
      Cnt := fRecent.Count;
    for i := 0 to Cnt - 1 do begin
      NewItem := TMenuItem.Create(Self);
      NewItem.Caption := fRecent.Strings[i];
      NewItem.Name := 'mMdReopenFile' + IntToStr(i);
      NewItem.Tag := i;
      NewItem.OnClick := MenuReopenClick;
      fMenuReopen.Add(NewItem);
    end;
    fMenuReopen.Enabled := True;
  end
  else begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := cEmpty;
    NewItem.Name := 'mMdReopenFile';
    NewItem.Enabled := False;
    fMenuReopen.Add(NewItem);
  end;
end;

procedure TDSFileManager.MenuOpenedClick(Sender: TObject);
begin
  ActiveFile := TFileHolder(fOpenedFiles[TMenuItem(Sender).Tag]);
end;

procedure TDSFileManager.MenuReopenClick(Sender: TObject);
begin
  Reopen(TMenuItem(Sender).Tag);
end;

function TDSFileManager.getActiveFile: TFileHolder;
begin
  Result := fActiveFile;
end;

function TDSFileManager.getActiveModel: TObject;
begin
 if (ActiveFile <> nil) then
   Result := ActiveFile.Model
 else
   Result := nil;
end;

procedure TDSFileManager.setActiveFile(const Value: TFileHolder);
begin
  if (fActiveFile <> Value) then begin
    DoBeforeFileChange;
    fActiveFile := Value;
    FileChanged;
  end;
end;

// ������� �������� ���������� ������� �� ini �����
procedure TDSFileManager.LoadRecentFromIni(aOnlyExisting: Boolean);
var
  recentReader: TRecentReader;
begin
  if not FileExists(IniFileName) then
    Exit;
  recentReader := TRecentReader.Create();
  recentReader.execute(IniFileName, fRecent, aOnlyExisting);
  recentReader.Free;
  UpdateMenuReopen;
end;

procedure TDSFileManager.SaveRecentToIni;
var
  I: Integer;
  IniF: TIniFile;
  TmpStr: string;
begin
  IniF := TIniFile.Create(IniFileName);
  try
    IniF.EraseSection(INISEC_RECENT);
    IniF.WriteInteger(INISEC_RECENT, INIDENT_COUNT, fRecent.Count);
    for I := 0 to fRecent.Count -1 do
    begin
      TmpStr := fRecent.Strings[I];
      if FileExists(TmpStr) then
        IniF.WriteString(INISEC_RECENT, INIDENT_ITEM + IntToStr(I), TmpStr);
    end;
  finally
    IniF.Free;
  end;
end;

procedure TDSFileManager.OpenedMenuUpdateChecked;
var
  i, activeIdx: Integer;
  Item: TMenuItem;
begin
  if (fMenuOpened <> nil) and (FileCount > 0) then begin
    activeIdx := fOpenedFiles.IndexOf(ActiveFile);
    for i := 0 to FileCount - 1 do
    begin
      Item := TMenuItem(FindComponent(cOpenedMenuName + IntToStr(i)));
      if (Item <> nil) then
        Item.Checked := i = activeIdx;
    end;
  end;
end;

procedure TDSFileManager.registerReader(aReader: TSaveLoader);
begin
  gSaveLoaderMgr.registerReader(aReader);
end;

procedure TDSFileManager.registerWriter(aWriter: TSaveLoader);
begin
  gSaveLoaderMgr.registerWriter(aWriter);
end;

procedure TDSFileManager.setAutoSave(const Value: Boolean);
begin
  fAutoSave := Value;
  fAutoSaveTimer.Enabled := Value;
end;

procedure TDSFileManager.setAutoSaveInterval(const Value: Integer);
begin
  fAutoSaveInterval := Value;
  fAutoSaveTimer.Interval := Value;
end;

procedure TDSFileManager.AutoSaveOnTimer(Sender: TObject);
var
  i: Integer;
  vIsNew: Boolean;
  vFileName, vFileExt: string;
begin
  Application.MainForm.Enabled := False;
  try
    for i := 0 to fOpenedFiles.Count - 1 do
    begin
      vIsNew := TFileHolder(fOpenedFiles[i]).isNew;
      if vIsNew then
      begin

      end
      else
      begin
        vFileName := TFileHolder(fOpenedFiles[i]).FileName;
        vFileExt := ExtractFileExt(vFileName);
        if vFileExt[1] = '.' then
          Delete(vFileExt, 1, 1);
        vFileName := ChangeFileExt(vFileName, '.~' + vFileExt);
        AutoSaveFile(TFileHolder(fOpenedFiles[i]), vFileName);
      end;
    end;
  finally
    Application.MainForm.Enabled := True;
  end;
end;

procedure TDSFileManager.AutoSaveFile(aFile: TFileHolder; aFileName: string);
var
  FileStream: TFileStream;
begin
  try
    FileStream := TFileStream.Create(aFileName, fmCreate or fmShareDenyWrite);
  except
    Exit;
  end;
  FileStream.Size := 0;
  try
    gSaveLoaderMgr.Save(aFile.Model, FileStream);
  except on E: Exception do
    // this can be full or write protected. For now we don't show anything
  end;
  FileStream.Free;
end;

function TDSFileManager.getKindForFile(aModel: TObject): TMdCustomFiles;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fFileKinds.Count - 1 do
    if fFileKinds[i].isFromClass(aModel) then
    begin
      Result := fFileKinds[i];
      Break;
    end;
end;

function TDSFileManager.createFile(aModel: TObject; FileName: string; aIsNew: boolean): TFileHolder;
var
  FileStream: TFileStream;
begin
  if not aIsNew then
    aIsNew := not FileExists(FileName);

  if (not aIsNew) then
  begin
    FileStream := TFileStream.Create(FileName, fmOpenRead);
    try
      FileStream.Position := 0;
      if not gSaveLoaderMgr.Load(aModel, FileStream) then begin
        Result := nil;
        Exit;
      end;
    finally
      FileStream.Free;
    end;
  end;

  Result := TFileHolder.Create;
  Result.Model := aModel;
  Result.FileName := FileName;
end;

function TDSFileManager.SaveToFile(aFileHolder: TFileHolder): Boolean;
var
  FileStream: TFileStream;
  driveLetter: String;
  drive: Byte;
  freeSpace: Int64;
begin
  driveLetter := ExtractFileDrive(aFileHolder.FileName);
  drive := 1 + Ord(driveLetter[1]) - Ord('A');
  freeSpace := DiskFree(drive);

  if (freeSpace >= 0) then begin
    if freeSpace < 1024*1024 then begin
      MessageDlg(cErrDiskSpace, mtError, [mbOk], 0);
      Result := False;
      Exit;
    end;
  end;

  if Assigned(fOnBeforeSave) then
    fOnBeforeSave(Self, aFileHolder);

  try
    if (aFileHolder.isNew) then
      FileStream := TFileStream.Create(aFileHolder.FileName, fmCreate)
    else
      FileStream := TFileStream.Create(aFileHolder.FileName, fmOpenReadWrite);
  except on E: Exception do begin
    MessageDlg(cErrDiskWrite, mtError, [mbOk], 0);
    Result := False;
    Exit;
  end;
  end;

  try
    FileStream.Size := 0;
    try
      gSaveLoaderMgr.Save(aFileHolder.Model, FileStream);
    except on E: Exception do begin
      Result := False;
      if E is EWriteError then begin
        MessageDlg(cErrDiskWrite, mtInformation, [mbOk], 0);
        Exit;
      end
      else begin
        MessageDlg(cErrDiskWrite, mtInformation, [mbOk], 0);
        Exit;
      end;
    end;
    end;

  finally
    FileStream.Free;
  end;
  Result := True;

end;

end.
