unit SaveLoader;

interface

uses
  Classes;

type

  TSaveLoader = class
  private
  protected
  public
    function AcceptVisitor(aVisited: TObject): Boolean; virtual; abstract;
    function execute(aModel: TObject; aStream: TStream): Boolean; virtual; abstract;

  end;

implementation

end.
