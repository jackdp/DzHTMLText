object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'DzHTMLText Example'
  ClientHeight = 328
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Lb: TDzHTMLText2
    Left = 8
    Top = 8
    Width = 313
    Height = 314
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    Text = 
      'Welcome to my app!<br><br><b>This is an important text</b> and <' +
      'i>this is italic text</i>.<br><u>But we have underline too</u>, ' +
      'and <s>strike-out</s> if you want.<br><br>You can change the <fn' +
      ':Courier New>font name</fn> and the <fs:14>font size</fs><br>The' +
      ' <fc:clBlue>colors</fc> are <bc:clYellow>allowed</bc> too!<br><b' +
      'r><c>Alignment, we have!</c><br><r>This is great</r><br><br>You ' +
      'can use tab align too:'#13#10'1<t:30>JOHN<t:100>100.000'#13#10'2<t:30>SARAH<' +
      't:100>150.000'#13#10'3<t:30>ERIC<t:100>180.000'#13#10#13#10'Click <a:www.google.' +
      'com.br>here to open Google</a>.'#13#10'Click <a:MSG_BOX>here to show a' +
      ' message box</a>.'
    AutoWidth = True
    AutoHeight = True
    OnLinkClick = LbLinkClick
    TagH1Params.Font.Charset = DEFAULT_CHARSET
    TagH1Params.Font.Color = clWindowText
    TagH1Params.Font.Height = -24
    TagH1Params.Font.Name = 'Arial'
    TagH1Params.Font.Style = [fsBold]
    TagH2Params.Font.Charset = DEFAULT_CHARSET
    TagH2Params.Font.Color = clWindowText
    TagH2Params.Font.Height = -20
    TagH2Params.Font.Name = 'Arial'
    TagH2Params.Font.Style = [fsBold]
    TagH3Params.Font.Charset = DEFAULT_CHARSET
    TagH3Params.Font.Color = clWindowText
    TagH3Params.Font.Height = -16
    TagH3Params.Font.Name = 'Arial'
    TagH3Params.Font.Style = [fsBold]
  end
end
