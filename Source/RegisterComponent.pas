unit RegisterComponent;

interface

uses
  Classes;

  procedure Register;

implementation

uses
  DSFileManager;

procedure Register;
begin
  RegisterComponents('DS', [TDSFileManager]);
end;

end.
