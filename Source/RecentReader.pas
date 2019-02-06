unit RecentReader;

interface

uses
  Classes;

const

  INISEC_RECENT = 'RECENT_FILES';
  INIDENT_COUNT = 'Count';
  INIDENT_ITEM = 'Item';

type

  TRecentReader = class
  public
    procedure execute(aFileName: string; aRecent: TStrings; aOnlyExisting: Boolean);

  end;

implementation

uses
  SysUtils, IniFiles;

procedure TRecentReader.execute(aFileName: string; aRecent: TStrings; aOnlyExisting: Boolean);
var
  i, Cnt: Integer;
  IniF: TIniFile;
  TmpStr: string;
begin
  if not FileExists(aFileName) then
    Exit;
    
  IniF := TIniFile.Create(aFileName);
  try
    Cnt := IniF.ReadInteger(INISEC_RECENT, INIDENT_COUNT, 0);
    for i := 0 to Cnt - 1 do
    begin
      TmpStr := IniF.ReadString(INISEC_RECENT, INIDENT_ITEM + IntToStr(i), '');
      if (not aOnlyExisting) or FileExists(TmpStr) then
        aRecent.Add(TmpStr);
    end;
  finally
    IniF.Free;
  end;
end;

end.
