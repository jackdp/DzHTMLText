unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DzHTMLText2, Vcl.StdCtrls, Vcl.ExtCtrls, System.Actions, Vcl.ActnList;

type

  TForm1 = class(TForm)
    sbox: TScrollBox;
    htt: TDzHTMLText2;
    pnBottom: TPanel;
    lblLinkInfo: TLabel;
    ActionList1: TActionList;
    actEsc: TAction;
    actReopenFile: TAction;
    procedure actEscExecute(Sender: TObject);
    procedure actReopenFileExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure httLinkEnter(Sender: TObject; LinkID: Integer; LinkData: TDHLinkData);
    procedure httLinkLeave(Sender: TObject; LinkID: Integer; LinkData: TDHLinkData);
  end;

var
  Form1: TForm1;
  TestFile: string;

implementation

{$R *.dfm}



procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'DzHTMLText2 - Example2';
  sbox.Align := alClient;
  sbox.BorderStyle := bsNone;
  pnBottom.BevelOuter := bvNone;
  lblLinkInfo.Align := alClient;
  Form1.Color := htt.Color;
  lblLinkInfo.Caption := '';
  TestFile := 'Text.HTMLText';
  actReopenFile.Execute;
  {$IFDEF DCC}Width := Width + 1;{$ENDIF} // force recreate htt bitmap
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta < 0 then sbox.VertScrollBar.Position := sbox.VertScrollBar.Position + 22
  else sbox.VertScrollBar.Position := sbox.VertScrollBar.Position - 22;
end;

procedure TForm1.httLinkEnter(Sender: TObject; LinkID: Integer; LinkData: TDHLinkData);
begin
  lblLinkInfo.Caption := LinkData.Target;
end;

procedure TForm1.httLinkLeave(Sender: TObject; LinkID: Integer; LinkData: TDHLinkData);
begin
  lblLinkInfo.Caption := '';
end;

procedure TForm1.actEscExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.actReopenFileExecute(Sender: TObject);
begin
  if FileExists(TestFile) then htt.LoadFromFile(TestFile);
end;

end.
