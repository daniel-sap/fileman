unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, ActnMan, ActnCtrls, ActnMenus, ExtCtrls, Menus, ActnList,
  ComCtrls, DSFileManager, StdCtrls, CustomFiles, ImgList;

type
  TForm2 = class(TForm)
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    ControlBar1: TControlBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    menuFile: TMenuItem;
    menuNew: TMenuItem;
    menuOpen: TMenuItem;
    menuOpened: TMenuItem;
    menuReopen: TMenuItem;
    actNew: TAction;
    actOpen: TAction;
    N1: TMenuItem;
    actSave: TAction;
    menuSave: TMenuItem;
    actSaveAs: TAction;
    actSaveAll: TAction;
    SaveAs1: TMenuItem;
    actClose: TAction;
    SaveAll1: TMenuItem;
    N2: TMenuItem;
    Close1: TMenuItem;
    actCloseAll: TAction;
    CloseAll1: TMenuItem;
    N3: TMenuItem;
    E1: TMenuItem;
    actExit: TAction;
    ToolButton6: TToolButton;
    ToolButton9: TToolButton;
    DSFileManager1: TDSFileManager;
    RichEdit1: TRichEdit;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveAllExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actCloseAllExecute(Sender: TObject);
    procedure DSFileManager1FileChanged(Sender: TObject);
    procedure RichEdit1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TDocumentFiles = class(TMdCustomFiles)
  public
    function CreateModel: TObject; override;

    function FileExt(out Description: string): string; override;
    function isFromClass(aModel: TObject): Boolean; override;
  end;

var
  Form2: TForm2;

implementation

uses
  DocumentModel, DocumentReader, DocumentWriter;

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  DSFileManager1.AppendFiles(TDocumentFiles.Create());
  DSFileManager1.registerReader(TDocumentReader.Create());
  DSFileManager1.registerWriter(TDocumentWriter.Create());
  DSFileManager1.IniFileName := ChangeFileExt(ParamStr(0), 'INI');
  DSFileManager1.LoadRecentFromIni();
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  DSFileManager1.SaveRecentToIni();
end;

procedure TForm2.actExitExecute(Sender: TObject);
begin
  Close();
end;

procedure TForm2.actNewExecute(Sender: TObject);
var
  document: TDocumentModel;
begin
  document := TDocumentModel.Create();
  document.Text := 'This is new document! Enter text here!';
  DSFileManager1.New(document);
end;

procedure TForm2.actOpenExecute(Sender: TObject);
begin
  DSFileManager1.Open();
end;

procedure TForm2.actSaveExecute(Sender: TObject);
begin
  DSFileManager1.Save();
end;

procedure TForm2.actSaveAsExecute(Sender: TObject);
begin
  DSFileManager1.SaveAs();
end;

procedure TForm2.actSaveAllExecute(Sender: TObject);
begin
  DSFileManager1.SaveAll();
end;

procedure TForm2.actCloseExecute(Sender: TObject);
begin
  DSFileManager1.Close();
end;

procedure TForm2.actCloseAllExecute(Sender: TObject);
begin
  DSFileManager1.CloseAll();
end;

procedure TForm2.DSFileManager1FileChanged(Sender: TObject);
begin
  Caption := 'DS File Manager Demo';
  if (DSFileManager1.ActiveFile = nil) then begin
    RichEdit1.Visible := False;
  end
  else begin
    Caption := Caption + ' | ' + DSFileManager1.ActiveFile.FileName;
    RichEdit1.Text := TDocumentModel(DSFileManager1.ActiveFile.Model).Text;
    RichEdit1.Visible := True;
  end;
end;

procedure TForm2.RichEdit1Change(Sender: TObject);
begin
  TDocumentModel(DSFileManager1.ActiveFile.Model).Text := RichEdit1.Text;
  DSFileManager1.ActiveFile.Modified := True;
end;

{ TDocumentFiles }

function TDocumentFiles.CreateModel: TObject;
begin
  Result := TDocumentModel.Create();
end;

function TDocumentFiles.FileExt(out Description: string): string;
begin
  Description := 'Text document(*.txt)';
  Result := 'Txt';
end;

function TDocumentFiles.isFromClass(aModel: TObject): Boolean;
begin
  Result := aModel is TDocumentModel;
end;

end.
