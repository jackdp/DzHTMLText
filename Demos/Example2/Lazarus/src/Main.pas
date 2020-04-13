unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ActnList, DzHTMLText2;

type

  TForm1 = class(TForm)
    actEsc: TAction;
    actReopenFile: TAction;
    ActionList1: TActionList;
    htt: TDzHTMLText2;
    lblLinkInfo: TLabel;
    pnBottom: TPanel;
    sbox: TScrollBox;
    procedure actEscExecute(Sender: TObject);
    procedure actReopenFileExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure httLinkEnter(Sender: TObject; LinkID: Integer; LinkData: TDHLinkData);
    procedure httLinkLeave(Sender: TObject; LinkID: Integer; LinkData: TDHLinkData);
  private

  public

  end;

var
  Form1: TForm1;
  TestFile: string;

implementation

{$R *.lfm}

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

