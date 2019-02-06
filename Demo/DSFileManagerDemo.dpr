program DSFileManagerDemo;

uses
  Forms,
  MainForm in 'MainForm.pas' {Form2},
  DocumentModel in 'DocumentModel.pas',
  DocumentReader in 'DocumentReader.pas',
  DocumentWriter in 'DocumentWriter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
