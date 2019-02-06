unit DocumentModel;

interface

type

  TDocumentModel = class
  private
    fText: string;
  public
    property Text: string read fText write fText;
  end;

implementation

end.
