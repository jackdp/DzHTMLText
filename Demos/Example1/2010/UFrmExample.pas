unit UFrmExample;

interface

uses Forms, Classes, Controls, DzHTMLText2;

type
  TForm1 = class(TForm)
    Lb: TDzHTMLText2;
    procedure LbLinkClick(Sender: TObject; LinkID: Integer;
      LinkData: TDHLinkData; var Handled: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Dialogs;

procedure TForm1.LbLinkClick(Sender: TObject; LinkID: Integer;
  LinkData: TDHLinkData; var Handled: Boolean);
begin
  if LinkData.Target='MSG_BOX' then
  begin
    ShowMessage('You have clicked at message box link!');
    Handled := True;
  end;
end;

end.
