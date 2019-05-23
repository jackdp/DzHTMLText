{------------------------------------------------------------------------------
TDzHTMLText component
Developed by Rodrigo Depiné Dalpiaz (digăo dalpiaz)
Label with formatting tags support

https://github.com/digao-dalpiaz/DzHTMLText

Please, read the documentation at GitHub link.

Supported Tags:
<A[:abc]></A> - Link
<B></B> - Bold
<I></I> - Italic
<U></U> - Underline
<S></S> - Strike out
<FN:abc></FN> - Font Name
<FS:123></FS> - Font Size
<FC:clcolor|$999999></FC> - Font Color
<BC:clcolor|$999999></BC> - Background Color
<BR> - Line Break
<L></L> - Align Left
<C></C> - Align Center
<R></R> - Aligh Right
<T:123> - Tab
<TF:123> - Tab with aligned break


--------------------------
Jacek Pazera
Last mod: 2019.05.23

Lazarus support

=== New tags ===
  <HR> - Horizontal line drawn in the color selected for the text with <fc> tag.
  <H1> - Header 1
  <H2> - Header 2
  <H3> - Header 3
  <LI> - List item - 1st level
  <LI2> - List item - 2nd level
  <LC> - Background line color drawn from the current position to the end of the current line
  <BBC> - Body background color drawn from the beginning of the current line to the end of the document
  <IMG> - Image
  <SUB> - Subscript
  <SUP> - Superscript

=== Additional changes ===

  Support for HTML colors:
    #FFAA88
    #ABC - short notation for #AABBCC
  RGB colors:
    rgb(50,100,150)
    rgb(50) - short notation for rgb(50,50,50)

  ReplaceForcedChars function renamed to ReplaceHtmlEntities
  Additional HTML entities: &euro; .. &SmallCircle;
  ExtraLineSpacing
  ExtraWordSpacing
  Internal margins: left and right
  Border - Warnig! Flickering when border width > 1. Place DzHTMLText on TPanel with DoubleBuffered set to True.
  Vertical alignment - Used when AutoHeight = False
  SaveToFile
  LoadFromFile
  SaveToBitmap
  SaveToBitmapFile
  AddPng
  AddPngFromFile
  GetPngImage
  GetPngImageFromFile
  PngCollection


  TODO:
  Text -> Lines
  BeginUpdate, EndUpdate
  TCustomDzHTMLText

------------------------------------------------------------------------------}



unit DzHTMLText;

{$IFDEF FPC}{$mode delphi}{$ENDIF}


interface


uses

{$IFDEF FPC}
  Controls, Classes, Messages, Generics.Collections, Graphics, LCLType, Types, LazarusPackageIntf, DzPngCollection;
{$ELSE}
  Vcl.Controls, System.Classes, Winapi.Messages, System.Generics.Collections, Vcl.Graphics, System.Types, DzPngCollection, Vcl.Imaging.pngimage;
{$ENDIF}

const
  TAG_ID_HR = 10; // Horizontal line
  TAG_ID_LC = 11; // Line color
  TAG_ID_LI = 12; // List item
  TAG_ID_LI2 = 13; // List item (2nd level)
  TAG_ID_IMG = 14; // Image
  TAG_ID_BBC = 15; // Body background color
  //TAG_ID_SUB = 16; // Subscript

type

  TDHLSSmallInt = -5..10; // jp: for ExtraLineSpacing
  TDHWSSmallInt = 0..5; // jp: for ExtraWordSpacing
  TDHVerticalAlignment = (vaTop, vaCenter, vaBottom);
  TDHBulletType = (btLongDash, btDash, btBullet, btCircle, btCustomString);

  {$region ' --- TDHWord --- '}
  {DHWord is an object to each word. Will be used to paint event.
  The words are separated by space/tag/line break.}
  TDHWord = class
  private
    Rect: TRect;
    Text: String;
    Group: Integer; //group number
    {The group is isolated at each line or tabulation to delimit text align area}
    Align: TAlignment;
    Font: TFont;
    BColor: TColor; //background color
    Link: Boolean; //is a link
    LinkID: Integer; //link number
    {The link number is created sequencially, when reading text links
    and works to know the link target, stored on a TStringList, because if
    the link was saved here at a work, it will be repeat if has multiple words
    per link, spending a lot of unnecessary memory.}
    Space: Boolean; //is an space

    Hover: Boolean; //the mouse is over the link
    //HorizontalLine: Boolean;
    ExtraColor: TColor;
    TagID: integer;
  public
    constructor Create;
    destructor Destroy; override;
  end;
  {$endregion TDHWord}

  TDHWordList = class(TObjectList<TDHWord>)
  private
    procedure Add(Rect: TRect; Text: String; Group: Integer; Align: TAlignment;
      Font: TFont; BColor: TColor; Link: Boolean; LinkID: Integer; Space: Boolean; ExtraColor: TColor = clNone; TagID: integer = -1);
  end;

  TDzHTMLText = class;

  TDHKindStyleLinkProp = (tslpNormal, tslpHover); //kind of link style

  {$region ' --- TDHStyleLinkProp --- '}
  {DHStyleLinkProp is a subproperty used at Object Inspector that contains
   link formatting when selected and not selected}
  TDHStyleLinkProp = class(TPersistent)
  private
    Lb: TDzHTMLText; //owner
    Kind: TDHKindStyleLinkProp;

    FFontColor: TColor;
    FBackColor: TColor;
    FUnderline: Boolean;
    procedure SetFontColor(const Value: TColor);
    procedure SetBackColor(const Value: TColor);
    procedure SetUnderline(const Value: Boolean);
    function GetDefaultFontColor: TColor;
    function GetStoredFontColor: Boolean;
    procedure SetPropsToCanvas(C: TCanvas); //method to use at paint event
    function GetStored: Boolean; //getstored general to use at owner
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(xLb: TDzHTMLText; xKind: TDHKindStyleLinkProp);
    procedure Assign(Source: TPersistent); override;
  published
    property FontColor: TColor read FFontColor write SetFontColor stored GetStoredFontColor;
    property BackColor: TColor read FBackColor write SetBackColor default clNone;
    property Underline: Boolean read FUnderline write SetUnderline default False;
  end;
  {$endregion TDHStyleLinkProp}

  {$region ' --- TDHLinkData --- '}
  TDHLinkData = class
  private
    FTarget: String;
    FText: String;
  public
    property Target: String read FTarget;
    property Text: String read FText;
  end;
  {$endregion TDHLinkData}

  TDHLinkDataList = class(TObjectList<TDHLinkData>);

  TDHEvLink = procedure(Sender: TObject; LinkID: Integer; LinkData: TDHLinkData) of object;
  TDHEvLinkClick = procedure(Sender: TObject; LinkID: Integer; LinkData: TDHLinkData; var Handled: Boolean) of object;


  {$region ' --- TDHTagHRParams --- '}
  TDHTagHRParams = class(TPersistent)
  private
    FStyle: TPenStyle;
    FLineHeight: integer;
    FOnChange: TNotifyEvent;
    procedure SetStyle(const Value: TPenStyle);
    procedure SetLineHeight(const Value: integer);
    procedure SetOnChange(const Value: TNotifyEvent);
  public
    constructor Create;
  published
    property Style: TPenStyle read FStyle write SetStyle default psSolid;
    property LineHeight: integer read FLineHeight write SetLineHeight default 1;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
 end;
 {$endregion TDHTagHRParams}

  {$region ' --- TDHTagHeaderParams --- '}
  TDHTagHeaderParams = class(TPersistent)
  private
    FFont: TFont;
    FOnChange: TNotifyEvent;
    FBackgroundColor: TColor;
    FAlignment: TAlignment;
    FTransparent: Boolean;
    procedure SetFont(const Value: TFont);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetTransparent(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Font: TFont read FFont write SetFont;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBtnFace;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion TDHTagHeaderParams}

  {$region ' --- TDHTagH1Params --- '}
  TDHTagH1Params = class(TDHTagHeaderParams)
  private
  public
    constructor Create;
  published
    property Font;
    property BackgroundColor;
    property Transparent;
    property Alignment;
    property OnChange;
  end;
  {$endregion TDHTagH1Params}

  {$region ' --- TDHTagH2Params --- '}
  TDHTagH2Params = class(TDHTagHeaderParams)
  private
  public
    constructor Create;
  published
    property Font;
    property BackgroundColor;
    property Transparent;
    property Alignment;
    property OnChange;
  end;
  {$endregion TDHTagH2Params}

  {$region ' --- TDHTagH3Params --- '}
  TDHTagH3Params = class(TDHTagHeaderParams)
  private
  public
    constructor Create;
  published
    property Font;
    property BackgroundColor;
    property Transparent;
    property Alignment;
    property OnChange;
  end;
  {$endregion TDHTagH3Params}

  {$region ' --- TDHInternalMargins --- '}
  TDHInternalMargins = class(TPersistent)
  private
    FLeft: integer;
    FRight: integer;
    FEnabled: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetLeft(const Value: integer);
    procedure SetRight(const Value: integer);
    procedure SetEnabled(const Value: Boolean);
    procedure SetOnChange(const Value: TNotifyEvent);
  public
    constructor Create;
  published
    property Left: integer read FLeft write SetLeft default 8;
    property Right: integer read FRight write SetRight default 8;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion TDHInternalMargins}

  {$region ' --- TDHTagLIParams --- '}
  TDHTagLIParams = class(TPersistent)
  private
    FMargin: integer;
    FOnChange: TNotifyEvent;
    FBulletType: TDHBulletType;
    FCustomString: string;
    FSpacing: Byte;
    procedure SetMargin(const Value: integer);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetBulletType(const Value: TDHBulletType);
    procedure SetCustomString(const Value: string);
    procedure SetSpacing(const Value: Byte); public
    constructor Create;
  published
    property Margin: integer read FMargin write SetMargin;
    property BulletType: TDHBulletType read FBulletType write SetBulletType default btBullet;
    property CustomString: string read FCustomString write SetCustomString;
    property Spacing: Byte read FSpacing write SetSpacing default 5;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion TDHTagLIParams}

  {$region ' --- TDHTagLI1Params --- '}
  TDHTagLI1Params = class(TDHTagLIParams)
  public
    constructor Create;
  published
    property Margin default 20;
    property BulletType default btBullet;
    property CustomString;
    property Spacing default 5;
    property OnChange;
  end;
  {$endregion TDHTagLI1Params}

  {$region ' --- TDHTagLI2Params --- '}
  TDHTagLI2Params = class(TDHTagLIParams)
  public
    constructor Create;
  published
    property Margin default 35;
    property BulletType default btDash;
    property CustomString;
    property Spacing default 5;
    property OnChange;
  end;
  {$endregion TDHTagLI2Params}

  {$region ' --- TDHBorder --- '}
  TDHBorder = class(TPersistent)
  private
    FPen: TPen;
    FOnChange: TNotifyEvent;
    FVisible: Boolean;
    procedure SetPen(const Value: TPen);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Pen: TPen read FPen write SetPen;
    property Visible: Boolean read FVisible write SetVisible default False;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion TDHBorder}

  {$region ' --- TDHTagSubParams --- '}
  TDHTagSubParams = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FPosYDelta: ShortInt;
    FFontSizeDelta: ShortInt;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetPosYDelta(const Value: ShortInt);
    procedure SetFontSizeDelta(const Value: ShortInt); public
    constructor Create;
  published
    property PosYDelta: ShortInt read FPosYDelta write SetPosYDelta default 0;
    property FontSizeDelta: ShortInt read FFontSizeDelta write SetFontSizeDelta default 0;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion TDHTagSubParams}

  {$region ' --- TDHTagSupParams --- '}
  TDHTagSupParams = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FPosYDelta: ShortInt;
    FFontSizeDelta: ShortInt;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetPosYDelta(const Value: ShortInt);
    procedure SetFontSizeDelta(const Value: ShortInt); public
    constructor Create;
  published
    property PosYDelta: ShortInt read FPosYDelta write SetPosYDelta default 0;
    property FontSizeDelta: ShortInt read FFontSizeDelta write SetFontSizeDelta default 0;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion TDHTagSupParams}


  {$region ' --- TDzHTMLText --- '}
  TDzHTMLText = class(TGraphicControl)
  private
    FAbout: String;

    LWords: TDHWordList; //word list to paint event
    LLinkData: TDHLinkDataList; //list of links info
    FBitmapTop: integer;
    FBitmapWidth: integer;
    FText: String;
    FAutoWidth: Boolean;
    FAutoHeight: Boolean;
    FMaxWidth: Integer; //max width when using AutoWidth
    //FTransparent: Boolean; //not used becaus of flickering
    FAutoOpenLink: Boolean; //link auto-open with ShellExecute

    FLines: Integer; //read-only
    FTextWidth: Integer; //read-only
    FTextHeight: Integer; //read-only

    FStyleLinkNormal, FStyleLinkHover: TDHStyleLinkProp;

    FOnLinkEnter, FOnLinkLeave: TDHEvLink;
    FOnLinkClick, FOnLinkRightClick: TDHEvLinkClick;

    FIsLinkHover: Boolean; //if has a selected link
    FSelectedLinkID: Integer; //selected link ID

    NoCursorChange: Boolean; //lock CursorChange event
    DefaultCursor: TCursor; //default cursor when not over a link
    FTagHRParams: TDHTagHRParams;
    FTagH1Params: TDHTagH1Params;
    FTagH2Params: TDHTagH2Params;
    FTagH3Params: TDHTagH3Params;
    FVerticalAlignment: TDHVerticalAlignment;
    FInternalMargins: TDHInternalMargins;
    FExtraLineSpacing: TDHLSSmallInt;
    FExtraWordSpacing: TDHWSSmallInt;
    FTagLIParams: TDHTagLI1Params;
    FTagLI2Params: TDHTagLI2Params;
    FBorder: TDHBorder;
    FTagSUBParams: TDHTagSubParams;
    FTagSUPParams: TDHTagSupParams;

    FPngCollection: TDzPngCollection;

    procedure SetText(const Value: String);
    procedure SetAutoHeight(const Value: Boolean);
    procedure SetAutoWidth(const Value: Boolean);
    procedure SetMaxWidth(const Value: Integer);

    function GetStoredStyleLink(const Index: Integer): Boolean;
    procedure SetStyleLink(const Index: Integer; const Value: TDHStyleLinkProp);

    procedure DoPaint;
    procedure Rebuild; //rebuild words
    procedure BuildAndPaint; //rebuild and repaint
    procedure CheckMouse(X, Y: Integer);
    procedure SetCursorWithoutChange(C: TCursor); //check links by mouse position
    procedure SetTagHRParams(const Value: TDHTagHRParams);

    procedure SetTagH1Params(const Value: TDHTagH1Params);
    procedure SetTagH2Params(const Value: TDHTagH2Params);
    procedure SetTagH3Params(const Value: TDHTagH3Params);

    procedure SetVerticalAlignment(const Value: TDHVerticalAlignment);

    procedure SetInternalMargins(const Value: TDHInternalMargins);
    procedure SetExtraLineSpacing(const Value: TDHLSSmallInt);
    procedure SetExtraWordSpacing(const Value: TDHWSSmallInt);
    procedure SetTagLIParams(const Value: TDHTagLI1Params);
    function GetBulletStr(const LIParams: TDHTagLIParams): string;

    procedure SetTagLI2Params(const Value: TDHTagLI2Params);
    procedure SetBorder(const Value: TDHBorder);
    procedure SetTagSUBParams(const Value: TDHTagSubParams);
    procedure SetTagSUPParams(const Value: TDHTagSupParams);

    procedure SetPngCollection(const Value: TDzPngCollection);
    //procedure SetTransparent(const Value: Boolean);
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure Click; override;
    procedure Resize; override;

    procedure CMColorchanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontchanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure CMCursorchanged(var Message: TMessage); message CM_CURSORCHANGED;
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property IsLinkHover: Boolean read FIsLinkHover;
    property SelectedLinkID: Integer read FSelectedLinkID;
    function GetLinkData(LinkID: Integer): TDHLinkData; //get data by link id
    function GetSelectedLinkData: TDHLinkData; //get data of selected link

    //jp
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToBitmap(const Bmp: {$IFDEF DCC}Vcl.{$ENDIF}Graphics.TBitmap);
    procedure SaveToBitmapFile(const BmpFile: string);
    function AddPngImage(const Image: TPngImage): integer; // Returns the index of the added image
    function AddPngImageFromFile(const PngFileName: string): integer; // Returns the index of the added image
    function GetPngImageFromFile(const PngFileName: string): TPngImage;
    function GetPngImage(const Index: integer): TPngImage;

  published
    property Align;
    property Anchors;
    property Color;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    {$IFDEF FPC}property BorderSpacing;{$ENDIF}

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF DCC}property OnGesture;{$ENDIF}
    {$IFDEF DCC}property OnMouseActivate;{$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;

    property Text: String read FText write SetText;
    //property Transparent: Boolean read FTransparent write SetTransparent default False;

    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth default False;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default False;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default 0;

    property StyleLinkNormal: TDHStyleLinkProp index 1 read FStyleLinkNormal write SetStyleLink stored GetStoredStyleLink;
    property StyleLinkHover: TDHStyleLinkProp index 2 read FStyleLinkHover write SetStyleLink stored GetStoredStyleLink;

    property Lines: Integer read FLines;
    property TextWidth: Integer read FTextWidth;
    property TextHeight: Integer read FTextHeight;

    property OnLinkEnter: TDHEvLink read FOnLinkEnter write FOnLinkEnter;
    property OnLinkLeave: TDHEvLink read FOnLinkLeave write FOnLinkLeave;
    property OnLinkClick: TDHEvLinkClick read FOnLinkClick write FOnLinkClick;
    property OnLinkRightClick: TDHEvLinkClick read FOnLinkRightClick write FOnLinkRightClick;

    property AutoOpenLink: Boolean read FAutoOpenLink write FAutoOpenLink default True;

    property About: String read FAbout;

    // jp
    property TagHRParams: TDHTagHRParams read FTagHRParams write SetTagHRParams;
    property TagH1Params: TDHTagH1Params read FTagH1Params write SetTagH1Params;
    property TagH2Params: TDHTagH2Params read FTagH2Params write SetTagH2Params;
    property TagH3Params: TDHTagH3Params read FTagH3Params write SetTagH3Params;

    // Vertical alignment of the internal bitmap in DzHTMLText control. Used only when AutoHeight = False
    property VerticalAlignment: TDHVerticalAlignment read FVerticalAlignment write SetVerticalAlignment default vaTop;
    property InternalMargins: TDHInternalMargins read FInternalMargins write SetInternalMargins;
    property ExtraLineSpacing: TDHLSSmallInt read FExtraLineSpacing write SetExtraLineSpacing default 0;
    property ExtraWordSpacing: TDHWSSmallInt read FExtraWordSpacing write SetExtraWordSpacing default 0;
    property TagLIParams: TDHTagLI1Params read FTagLIParams write SetTagLIParams;
    property TagLI2Params: TDHTagLI2Params read FTagLI2Params write SetTagLI2Params;
    property Border: TDHBorder read FBorder write SetBorder;
    property TagSUBParams: TDHTagSubParams read FTagSUBParams write SetTagSUBParams;
    property TagSUPParams: TDHTagSupParams read FTagSUPParams write SetTagSUPParams;
    property PngCollection: TDzPngCollection read FPngCollection write SetPngCollection;
  end;
  {$endregion TDzHTMLText}


procedure Register;

implementation

uses
{$IFDEF FPC}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF} SysUtils, LCLIntf;
{$ELSE}
  System.SysUtils, System.UITypes, Winapi.Windows, Winapi.ShellAPI;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Digao', [TDzHTMLText]);
end;



{$region ' ---------------------- TDHWord ------------------- '}
constructor TDHWord.Create;
begin
  inherited;
  Font := TFont.Create;
end;

destructor TDHWord.Destroy;
begin
  Font.Free;
  inherited;
end;
{$endregion TDHWord}

procedure TDHWordList.Add(Rect: TRect; Text: String; Group: Integer; Align: TAlignment;
  Font: TFont; BColor: TColor; Link: Boolean; LinkID: Integer; Space: Boolean; ExtraColor: TColor; TagID: integer);
var W: TDHWord;
begin
  W := TDHWord.Create;
  inherited Add(W);

  W.Rect := Rect;
  W.Text := Text;
  W.Group := Group;
  W.Align := Align;
  W.Font.Assign(Font);
  W.BColor := BColor;
  W.Link := Link;
  W.LinkID := LinkID;
  W.Space := Space;
  W.ExtraColor := ExtraColor;
  W.TagID := TagID;
end;



{$region ' -------------------------------------------- TDzHTMLText / TBuilder (part 1) ------------------------------------------------ '}
constructor TDzHTMLText.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  //Warning! The use of transparency in the component causes flickering

  FAbout := 'Digăo Dalpiaz / Version 1.0';

  FStyleLinkNormal := TDHStyleLinkProp.Create(Self, tslpNormal);
  FStyleLinkHover := TDHStyleLinkProp.Create(Self, tslpHover);
  LWords := TDHWordList.Create;
  LLinkData := TDHLinkDataList.Create;

  FAutoOpenLink := True;

  FSelectedLinkID := -1;

  DefaultCursor := Cursor;


  // jp

  {$IFDEF FPC}
  // default values: Black, Width 5, Height 5
  // So we have to set some better values:
  Color := clBtnFace;
  Width := 200;
  Height := 200;
  {$ENDIF}

  FTagHRParams := TDHTagHRParams.Create;
  FTagHRParams.OnChange := PropsChanged;

  FTagH1Params := TDHTagH1Params.Create;
  FTagH1Params.OnChange := PropsChanged;

  FTagH2Params := TDHTagH2Params.Create;
  FTagH2Params.OnChange := PropsChanged;

  FTagH3Params := TDHTagH3Params.Create;
  FTagH3Params.OnChange := PropsChanged;

  FVerticalAlignment := vaTop;

  FInternalMargins := TDHInternalMargins.Create;
  FInternalMargins.OnChange := PropsChanged;

  FExtraLineSpacing := 0;

  FTagLIParams := TDHTagLI1Params.Create;
  FTagLIParams.OnChange := PropsChanged;

  FTagLI2Params := TDHTagLI2Params.Create;
  FTagLI2Params.OnChange := PropsChanged;

  FBorder := TDHBorder.Create;
  FBorder.OnChange := PropsChanged;
  FBorder.Visible := False;

  FTagSUBParams := TDHTagSubParams.Create;
  FTagSUBParams.OnChange := PropsChanged;

  FTagSUPParams := TDHTagSupParams.Create;
  FTagSUPParams.OnChange := PropsChanged;

  FPngCollection := nil;
end;

destructor TDzHTMLText.Destroy;
begin
  FStyleLinkNormal.Free;
  FStyleLinkHover.Free;
  LWords.Free;
  LLinkData.Free;
  FTagHRParams.Free;
  FTagH1Params.Free;
  FTagH2Params.Free;
  FTagH3Params.Free;
  FInternalMargins.Free;
  FTagLIParams.Free;
  FTagLI2Params.Free;
  FBorder.Free;
  FTagSUBParams.Free;
  FTagSUPParams.Free;
  inherited;
end;

procedure TDzHTMLText.Loaded;
begin
  {Warning! When a component is inserted at design-time, the Loaded
  is not fired, because there is nothing to load. The Loaded is only fired
  when loading component that already has saved properties on DFM file.}
  inherited;
  Rebuild;
end;


function TDzHTMLText.AddPngImage(const Image: TPngImage): integer;
var
  Item: TDzPngCollectionItem;
begin
  Result := -1;
  if not Assigned(FPngCollection) then Exit;
  Item := FPngCollection.Items.Add;
  Item.PngImage.Assign(Image);
  Result := Item.Index;
end;

function TDzHTMLText.AddPngImageFromFile(const PngFileName: string): integer;
var
  Png: TPngImage;
begin
  Result := -1;
  if not FileExists(PngFileName) then Exit;
  Png := TPngImage.Create;
  try
    Png.LoadFromFile(PngFileName);
    Result := AddPngImage(Png);
  except
    // Eating an exception!
  end;
end;

procedure TDzHTMLText.BuildAndPaint;
begin
  //Rebuild words and repaint
  Rebuild;
  Invalidate;
end;

procedure TDzHTMLText.SaveToBitmap(const Bmp: {$IFDEF DCC}Vcl.{$ENDIF}Graphics.TBitmap);
begin
  Bmp.SetSize(Canvas.ClipRect.Right, Canvas.ClipRect.Bottom);
  BitBlt(Bmp.Canvas.Handle, 0, 0, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TDzHTMLText.SaveToBitmapFile(const BmpFile: string);
var
  Bmp: {$IFDEF DCC}Vcl.{$ENDIF}Graphics.TBitmap;
begin
  Bmp := {$IFDEF DCC}Vcl.{$ENDIF}Graphics.TBitmap.Create;
  try
    SaveToBitmap(Bmp);
    Bmp.SaveToFile(BmpFile);
  finally
    Bmp.Free;
  end;
end;

procedure TDzHTMLText.SaveToFile(const FileName: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := FText;
    sl.SaveToFile(FileName, TEncoding.UTF8);
  finally
    sl.Free;
  end;
end;

procedure TDzHTMLText.LoadFromFile(const FileName: string);
var
  sl: TStringList;
begin
  FText := '';
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    FText := sl.Text;
    BuildAndPaint;
  finally
    sl.Free;
  end;
end;

procedure TDzHTMLText.SetAutoHeight(const Value: Boolean);
begin
  if Value<>FAutoHeight then
  begin
    FAutoHeight := Value;

    if Value then Rebuild;
  end;
end;

procedure TDzHTMLText.SetAutoWidth(const Value: Boolean);
begin
  if Value<>FAutoWidth then
  begin
    FAutoWidth := Value;

    if Value then Rebuild;
  end;
end;

procedure TDzHTMLText.SetBorder(const Value: TDHBorder);
begin
  FBorder := Value;
  PropsChanged(Self);
end;

procedure TDzHTMLText.SetMaxWidth(const Value: Integer);
begin
  if Value<>FMaxWidth then
  begin
    FMaxWidth := Value;

    Rebuild;
  end;
end;

procedure TDzHTMLText.SetPngCollection(const Value: TDzPngCollection);
begin
  FPngCollection := Value;
end;

procedure TDzHTMLText.SetText(const Value: String);
begin
  if Value<>FText then
  begin
    FText := Value;

    BuildAndPaint;
  end;
end;

procedure TDzHTMLText.SetVerticalAlignment(const Value: TDHVerticalAlignment);
begin
  if FVerticalAlignment = Value then Exit;
  FVerticalAlignment := Value;
  BuildAndPaint;
end;

{procedure TDzHTMLText.SetTransparent(const Value: Boolean);
begin
  if Value<>FTransparent then
  begin
    FTransparent := Value;

    Invalidate;
  end;
end;}

procedure TDzHTMLText.CMColorchanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TDzHTMLText.CMFontchanged(var Message: TMessage);
begin
  BuildAndPaint;
end;

procedure TDzHTMLText.Resize;
begin
  //on component creating, there is no parent and the resize is fired,
  //so, the canvas is not present at this moment.
  if HasParent then
    Rebuild;

  inherited;
end;

procedure TDzHTMLText.PropsChanged(Sender: TObject);
begin
  //Invalidate;
  BuildAndPaint;
end;


// jp
procedure DrawLineEx(Canvas: TCanvas; Rect: TRect; LineHeight: integer; bVerticalCenter: Boolean = True);
var
  OldWidth, LHeight, xhs: integer;
begin
  OldWidth := Canvas.Pen.Width;
  Canvas.Pen.Width := 1;
  LHeight := LineHeight;

  if bVerticalCenter then xhs := (Rect.Height div 2) - (LineHeight div 2)
  else xhs := 0;

  while LHeight > 0 do
  begin
    Dec(LHeight);
    Canvas.MoveTo(Rect.Left, Rect.Top + xhs);
    Canvas.LineTo(Rect.Right, Rect.Top + xhs);
    Inc(Rect.Top);
  end;

  Canvas.Pen.Width := OldWidth;
end;

procedure JppFrame3D(Canvas: TCanvas; Rect: TRect; LeftColor, RightColor, TopColor, BottomColor: TColor; Width: Integer); overload;

  procedure DoRect;
  begin
    with Canvas, Rect do
    begin
      Pen.Color := TopColor;
      MoveTo(Left, Top);
      LineTo(Right, Top);

      Pen.Color := RightColor;
      LineTo(Right, Bottom);

      Pen.Color := BottomColor;
      LineTo(Left, Bottom);

      Pen.Color := LeftColor;
      LineTo(Left, Top);
    end;
  end;

begin
  Canvas.Pen.Width := 1;

  Dec(Rect.Bottom);
  Dec(Rect.Right);

  while Width > 0 do
  begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;

  Inc(Rect.Bottom);
  Inc(Rect.Right);
end;

procedure JppFrame3D(Canvas: TCanvas; Rect: TRect; Color: TColor; Width: integer); overload;
begin
  JppFrame3D(Canvas, Rect, Color, Color, Color, Color, Width);
end;

procedure TDzHTMLText.Paint;
begin
  inherited;
  DoPaint;
end;


  {$region ' --- TDzHTMLText.DoPaint --- '}
procedure TDzHTMLText.DoPaint;
var
  W: TDHWord;
  B: {$IFDEF DCC}Vcl.{$ENDIF}Graphics.TBitmap;
  R: TRect;
  BitmapYPos, BitmapXPos: integer;
  s: string;
  dx, x: integer;
  bcv: TCanvas;
  Png: TPngImage;
begin

  //Using internal bitmap as a buffer to reduce flickering
  B := {$IFDEF DCC}Vcl.{$ENDIF}Graphics.TBitmap.Create;
  try

    B.SetSize(FBitmapWidth, Height);
    bcv := B.Canvas;
    //FBitmapWidth := B.Width;

    //if not FTransparent then
    //begin
      bcv.Brush.Color := Color;
      bcv.FillRect(ClientRect);
    //end;

    if (csDesigning in ComponentState) and (not FBorder.Visible) then
    begin
      bcv.Pen.Style := psDot;
      bcv.Pen.Color := clBtnShadow;
      bcv.Brush.Style := bsClear;
      bcv.Rectangle(ClientRect);
    end;

    for W in LWords do
    begin

      bcv.Font.Assign(W.Font);

      if W.BColor <> clNone then bcv.Brush.Color := W.BColor
      else bcv.Brush.Style := bsClear;


      // ------------ <LC> - Line color -------------
      if W.TagID = TAG_ID_LC then
        if W.ExtraColor <> clNone then
        begin
          bcv.Brush.Style := bsSolid;
          bcv.Brush.Color := W.ExtraColor;
          R := W.Rect;
          R.Width := FBitmapWidth;
          //R.Left := 0;
          //bcv.Font.Assign(W.Font);
          //R.Bottom := R.Top + bcv.TextHeight('1');
          bcv.FillRect(R);
          Continue;
        end;

      // ------------ <BBC> - Body background color -------------
      if W.TagID = TAG_ID_BBC then
        if W.ExtraColor <> clNone then
        begin
          bcv.Brush.Style := bsSolid;
          bcv.Brush.Color := W.ExtraColor;
          R := W.Rect;
          R.Left := 0;
          R.Width := FBitmapWidth;
          R.Height := Height;
          bcv.FillRect(R);
          Continue;
        end;

      // ------------------ <IMG> ------------------
      if W.TagID = TAG_ID_IMG then
        begin

          Png := nil;

          if TryStrToInt(W.Text, x) then
          begin
            Png := GetPngImage(x);
            if Png = nil then Continue;
            if Png.Empty then Continue;
          end

          else if FileExists(W.Text) then
          begin
            Png := GetPngImageFromFile(W.Text);
            if Png = nil then Continue;
          end;

          if Assigned(Png) and (not Png.Empty) then
          begin
            case W.Align of
              taCenter: dx := W.Rect.Left - (Png.Width div 2);
              taRightJustify: dx := W.Rect.Right - Png.Width
            else
              dx := W.Rect.Left;
            end;
            bcv.Draw(dx, W.Rect.Top, Png);
          end;

          Continue;
        end;

      // ---------------- <HR> -------------------
      if W.TagID  = TAG_ID_HR then
      begin
        bcv.Pen.Color := bcv.Font.Color;
        bcv.Pen.Width := 1;
//        bcv.Pen.Style := psDot;
//        bcv.Rectangle(W.Rect);
        bcv.Pen.Style := FTagHRParams.Style;

        R := W.Rect;
//        R.Left := W.Rect.Left + MarginLeft;
//        R.Width := W.Rect.Width - MarginRight - MarginLeft;
        DrawLineEx(bcv, R, FTagHRParams.LineHeight, True);
        Continue;
      end;

      R := W.Rect;

      if W.TagID = TAG_ID_LI then
      begin
        if W.Text <> '' then s := W.Text
        else s := GetBulletStr(FTagLIParams);
        bcv.TextOut(R.Left - FTagLIParams.Spacing - bcv.TextWidth(s), R.Top, s);
        Continue;
      end;

      if W.TagID = TAG_ID_LI2 then
      begin
        if W.Text <> '' then s := W.Text
        else s := GetBulletStr(FTagLI2Params);
        bcv.TextOut(R.Left - FTagLI2Params.Spacing - bcv.TextWidth(s), R.Top, s);
        Continue;
      end;


      if W.Link then
        if W.Hover then //selected
          FStyleLinkHover.SetPropsToCanvas(bcv)
        else
          FStyleLinkNormal.SetPropsToCanvas(bcv);

      //bcv.Brush.Color := RandomColor;

      DrawText(bcv.Handle, {$IFDEF FPC}PChar({$ENDIF}W.Text{$IFDEF FPC}){$ENDIF}, -1, R, DT_NOCLIP or DT_NOPREFIX or DT_BOTTOM or DT_SINGLELINE);
      {Using DrawText, because TextOut has not clip option, which causes
      bad overload of text when painting using background, oversizing the
      text area wildly.}

    end; // for W


    // Cleaning the Canvas. To reduce flickering, called just before drawing a bitmap on Canvas. // jp
    with Canvas do
    begin
      Brush.Color := Color;
      Brush.Style := bsSolid;
      Pen.Color := Color;
      Rectangle(Self.ClientRect);
    end;


    // ------------------------------ Draw HTML bitmap on Canvas ----------------------------------
    FBitmapTop := 0;
    if FInternalMargins.Enabled then BitmapXPos := FInternalMargins.Left
    else BitmapXPos := 0;
    BitmapYPos := 0;
    if FAutoHeight then Canvas.Draw(BitmapXPos, BitmapYPos, B) //to reduce flickering
    else
    begin
      case FVerticalAlignment of
        vaTop: BitmapYPos := 0;
        vaCenter:
         if FTextHeight < Self.Height then BitmapYPos := (Self.Height div 2) - (FTextHeight div 2)
         else BitmapYPos := 0;
        vaBottom:
          if FTextHeight < Self.Height then BitmapYPos := Self.Height - FTextHeight
          else BitmapYPos := 0;
      else
        BitmapYPos := 0; // <-- supress stupid warning
      end;
      Canvas.Draw(BitmapXPos, BitmapYPos, B);
      FBitmapTop := BitmapYPos;
    end;


    // To reduce flickering place DzHTMLText control on the TPanel with DoubleBuffers = True
    if FBorder.Visible then
    begin
      Canvas.Pen.Assign(FBorder.Pen);
      JppFrame3D(Canvas, ClientRect, FBorder.Pen.Color, FBorder.Pen.Width);
    end;




  finally
    B.Free;
  end;



end;
  {$endregion TDzHTMLText.DoPaint}

function TDzHTMLText.GetBulletStr(const LIParams: TDHTagLIParams): string;
begin
  case LIParams.BulletType of
    btBullet: Result := '•';
    btLongDash: Result := '–';
    btDash: Result := '-';
    btCircle: Result := '∘';
  else
    Result := LIParams.CustomString;
  end;
end;

function TDzHTMLText.GetLinkData(LinkID: Integer): TDHLinkData;
begin
  Result := LLinkData[LinkID];
end;

function TDzHTMLText.GetPngImageFromFile(const PngFileName: string): TPngImage;
begin
  Result := nil;
  if not FileExists(PngFileName) then Exit;
  Result := TPngImage.Create;
  try
    Result.LoadFromFile(PngFileName);
  except
    Result := nil;
  end;
end;

function TDzHTMLText.GetPngImage(const Index: integer): TPngImage;
begin
  Result := nil;
  if not Assigned(FPngCollection) then Exit;
  if Index < 0 then Exit;
  if Index > FPngCollection.Count - 1 then Exit;
  Result := FPngCollection.Items[Index].PngImage;
end;

function TDzHTMLText.GetSelectedLinkData: TDHLinkData;
begin
  Result := LLinkData[FSelectedLinkID];
end;

procedure TDzHTMLText.CMCursorchanged(var Message: TMessage);
begin
  if NoCursorChange then Exit;

  DefaultCursor := Cursor; //save default cursor to when link not selected
end;

procedure TDzHTMLText.SetCursorWithoutChange(C: TCursor);
begin
  //Set cursor, but without fire cursos change event
  NoCursorChange := True;
  try
    Cursor := C;
  finally
    NoCursorChange := False;
  end;
end;


procedure TDzHTMLText.SetExtraLineSpacing(const Value: TDHLSSmallInt);
begin
  if FExtraLineSpacing = Value then Exit;
  FExtraLineSpacing := Value;
  PropsChanged(Self);
end;

procedure TDzHTMLText.SetExtraWordSpacing(const Value: TDHWSSmallInt);
begin
  if FExtraWordSpacing = Value then Exit;
  FExtraWordSpacing := Value;
  PropsChanged(Self);
end;

procedure TDzHTMLText.SetInternalMargins(const Value: TDHInternalMargins);
begin
  FInternalMargins := Value;
  FInternalMargins.OnChange := PropsChanged;
end;

procedure TDzHTMLText.SetTagH1Params(const Value: TDHTagH1Params);
begin
  FTagH1Params := Value;
  Invalidate;
end;

procedure TDzHTMLText.SetTagH2Params(const Value: TDHTagH2Params);
begin
  FTagH2Params := Value;
  Invalidate;
end;

procedure TDzHTMLText.SetTagH3Params(const Value: TDHTagH3Params);
begin
  FTagH3Params := Value;
  Invalidate;
end;

procedure TDzHTMLText.SetTagHRParams(const Value: TDHTagHRParams);
begin
  FTagHRParams := Value;
  Invalidate;
end;

procedure TDzHTMLText.SetTagLI2Params(const Value: TDHTagLI2Params);
begin
  FTagLI2Params := Value;
  PropsChanged(Self);
end;

procedure TDzHTMLText.SetTagLIParams(const Value: TDHTagLI1Params);
begin
  FTagLIParams := Value;
  PropsChanged(Self);
end;

procedure TDzHTMLText.SetTagSUBParams(const Value: TDHTagSubParams);
begin
  FTagSUBParams := Value;
  PropsChanged(Self);
end;

procedure TDzHTMLText.SetTagSUPParams(const Value: TDHTagSupParams);
begin
  FTagSUPParams := Value;
  PropsChanged(Self);
end;

  {$Region ' ------------- Mouse related procs --------------- '}
procedure TDzHTMLText.CheckMouse(X, Y: Integer);
var FoundHover, HasChange, Old: Boolean;
    LinkID: Integer;
    W: TDHWord;
    dx, dy: integer;
begin
  FoundHover := False;
  HasChange := False;
  LinkID := -1;

  dy := Y - FBitmapTop;
  if FInternalMargins.Enabled then dx := X - FInternalMargins.Left
  else dx := X;

  //find the first word, if there is any
  for W in LWords do
    if W.Link then
    begin
      if W.Rect.Contains({$IFDEF FPC}Types.{$ENDIF}Point(dx, dy)) then //selected
      begin
          FoundHover := True; //found word of a link selected
          LinkID := W.LinkID;

          Break;
      end;
    end;

  //set as selected all the words of same link, and unselect another links
  for W in LWords do
    if W.Link then
    begin
      Old := W.Hover;
      W.Hover := (W.LinkID = LinkID);
      if Old<>W.Hover then HasChange := True; //changed
    end;

  if HasChange then //there is any change
  begin
    if FoundHover then //enter the link
    begin
      SetCursorWithoutChange(crHandPoint); //set HandPoint cursor
      FIsLinkHover := True;
      FSelectedLinkID := LinkID;
      if Assigned(FOnLinkEnter) then
        FOnLinkEnter(Self, LinkID, LLinkData[LinkID]);
    end else
    begin //leave the link
      SetCursorWithoutChange(DefaultCursor); //back to default cursor
      FIsLinkHover := False;
      LinkID := FSelectedLinkID; //save to use on OnLinkLeave event
      FSelectedLinkID := -1;
      if Assigned(FOnLinkLeave) then
        FOnLinkLeave(Self, LinkID, LLinkData[LinkID]);
    end;

    Invalidate;
  end;
end;

procedure TDzHTMLText.Click;
var
  Handled: Boolean;
  Target: string;
begin
  if FIsLinkHover then
  begin
    Handled := False;
    if Assigned(FOnLinkClick) then
      FOnLinkClick(Self, FSelectedLinkID, LLinkData[FSelectedLinkID], Handled);

    if FAutoOpenLink and not Handled then
    begin
      Target := LLinkData[FSelectedLinkID].FTarget;
      {$IFDEF MSWINDOWS}ShellExecute(0, '', PChar(Target), '', '', 0);
      {$ELSE}
      if (Copy(Target, 1, 4) = 'http') or (Copy(Target, 1, 4) = 'www.') then OpenURL(Target)
      else OpenDocument(Target);
      {$ENDIF}
    end;
  end;

  inherited;
end;

procedure TDzHTMLText.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var Handled: Boolean;
begin
  if Button = mbRight then
    if IsLinkHover then
      if Assigned(FOnLinkRightClick) then
      begin
        Handled := False;
        FOnLinkRightClick(Self, FSelectedLinkID, LLinkData[FSelectedLinkID], Handled);
      end;

  inherited;
end;

procedure TDzHTMLText.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  CheckMouse(X, Y);

  inherited;
end;

procedure TDzHTMLText.CMMouseLeave(var Message: TMessage);
begin
  //Mouse leaves the componente
  CheckMouse(-1, -1);

  inherited;
end;

//

type
  TTokenKind = (
    ttInvalid,
    ttBold, ttItalic, ttUnderline, ttStrike,
    ttFontName, ttFontSize, ttFontColor, ttBackColor,
    ttTab, ttTabF, ttSpace,
    ttBreak, ttText, ttLink,
    ttAlignLeft, ttAlignCenter, ttAlignRight,
    // jp
    ttHorizontalLine, ttHeader1, ttHeader2, ttHeader3, ttLineColor, ttListItem, ttListItem2, ttImage,
    ttBodyBackgroundColor {<bbc>}, ttSubscript, ttSuperscript
  );

  TToken = class
    Kind: TTokenKind;
    TagClose: Boolean;
    Text: String;
    Value: Integer;
  end;

  TListToken = class(TObjectList<TToken>)
    function GetLinkText(IEnd: Integer): String;
  end;

  TBuilder = class
  private
    Lb: TDzHTMLText;
    L: TListToken;
    LGroupBound: TList<Integer>; //bounds list of the group
    {The list of created with the X position of limit where the group ends
     to use on text align until the group limit}

    CalcWidth, CalcHeight: Integer; //width and height to set at component when using auto

    function ProcessTag(Tag: String): Boolean;
    procedure AddToken(aKind: TTokenKind; aTagClose: Boolean = False; aText: String = ''; aValue: Integer = 0);
    procedure BuildTokens; //create list of tokens
    procedure BuildWords; //create list of words
    procedure CheckAligns; //realign words
  public
    constructor Create;
    destructor Destroy; override;
  end;
  {$endregion Mouse related procs}

constructor TBuilder.Create;
begin
  inherited;
  L := TListToken.Create;
  LGroupBound := TList<Integer>.Create;
end;

destructor TBuilder.Destroy;
begin
  L.Free;
  LGroupBound.Free;
  inherited;
end;

procedure TDzHTMLText.Rebuild;
var B: TBuilder;
begin
  if csLoading in ComponentState then Exit;

  LWords.Clear; //clean old words
  LLinkData.Clear; //clean old links

  if FInternalMargins.Enabled then FBitmapWidth := Width - FInternalMargins.Left - FInternalMargins.Right
  else FBitmapWidth := Width;

  B := TBuilder.Create;
  try
    B.Lb := Self;

    B.BuildTokens;
    B.BuildWords;
    B.CheckAligns;

    FTextWidth := B.CalcWidth;
    FTextHeight := B.CalcHeight;

    if FAutoWidth then Width := B.CalcWidth;
    if FAutoHeight then Height := B.CalcHeight;

  finally
    B.Free;
  end;
end;
{$endregion TDzHTMLText}



function ReplaceHtmlEntities(A: String): String;
var
  rf: TReplaceFlags;
begin
  rf := [rfReplaceAll {, rfIgnoreCase}];  // HTML entities are case sensitive

  //Allow tag characters at text
  A := StringReplace(A, '&lt;', '<', rf);
  A := StringReplace(A, '&gt;', '>', rf);

  A := StringReplace(A, '&euro;', '€', rf);
  A := StringReplace(A, '&cent;', '¢', rf);
  A := StringReplace(A, '&pound;', '£', rf);
  A := StringReplace(A, '&yen;', '¥', rf);
  A := StringReplace(A, '&amp;', '&', rf);
  A := StringReplace(A, '&copy;', '©', rf);
  A := StringReplace(A, '&reg;', '®', rf);
  A := StringReplace(A, '&sect;', '§', rf);
  A := StringReplace(A, '&deg;', '°', rf);
  A := StringReplace(A, '&sup2;', '²', rf);
  A := StringReplace(A, '&sup3;', '³', rf);
  A := StringReplace(A, '&Integral;', '∫', rf);

  A := StringReplace(A, '&micro;', 'µ', rf);
  A := StringReplace(A, '&para;', '¶', rf);
  A := StringReplace(A, '&middot;', '·', rf);
  A := StringReplace(A, '&plusmn;', '±', rf);
  A := StringReplace(A, '&times;', '×', rf);
  A := StringReplace(A, '&divide;', '÷', rf);
  A := StringReplace(A, '&Omega;', 'Ω', rf);
  A := StringReplace(A, '&alpha;', 'α', rf);
  A := StringReplace(A, '&beta;', 'β', rf);
  A := StringReplace(A, '&gamma;', 'γ', rf);
  A := StringReplace(A, '&Gamma;', 'Γ', rf);
  A := StringReplace(A, '&delta;', 'δ', rf);
  A := StringReplace(A, '&Delta;', 'Δ', rf);
  A := StringReplace(A, '&pi;', 'π', rf);
  A := StringReplace(A, '&Pi;', 'Π', rf);
  A := StringReplace(A, '&Sigma;', 'Σ', rf);
  A := StringReplace(A, '&bull;', '•', rf);
  A := StringReplace(A, '&ndash;', '–', rf);
  A := StringReplace(A, '&trade;', '™', rf);
  A := StringReplace(A, '&SmallCircle;', '∘', rf); // &#8728; / &#x02218;

  Result := A;
end;

{$region ' ------------ from JPLib: JPL.Colors, JPL.Conversion, JPL.Strings ---------- ' }
function TryHexToInt(Hex: string; var xInt: Int64): Boolean;
var
  Code: integer;
  i64: int64;
begin
  Result := True;
  if Hex = '' then Exit(False);
  if Hex[1] <> '$' then Hex := '$' + Hex;
  Val(Hex, i64, Code);
  if Code = 0 then xInt := i64
  else Result := False;
end;

function TryHexToByte(Hex: string; var xb: Byte): Boolean;
var
  x: Int64;
begin
  Result := False;
  x := 0;
  if not TryHexToInt(Hex, x) then Exit;
  if (x < 0) or (x > 255) then Exit;
  Result := True;
  xb := Byte(x);
end;

function TryHtmlStrToColor(s: string; out Color: TColor): Boolean; overload;
var
  sr, sg, sb: string;
  r, g, b: Byte;
  c1, c2, c3: Char;
  xLen: integer;

  function RGB(const R, G, B: Byte): TColor;
  begin
    Result := (R or (G shl 8) or (B shl 16));
  end;

begin
  r := 0; g := 0; b := 0;
  Result := False;

  s := StringReplace(s, ' ', '', [rfReplaceAll]);
  s := StringReplace(s, '#', '', [rfReplaceAll]);

  xLen := Length(s);
  // short RGB notation (#RGB)
  // short to long conversion
  if xLen = 3 then
  begin
    c1 := s[1];
    c2 := s[2];
    c3 := s[3];
    s := c1 + c1 + c2 + c2 + c3 + c3;
  end;

  xLen := Length(s);
  // long RGB notation (#RRGGBB)
  if xLen = 6 then
  begin
    sr := Copy(s, 1, 2);
    if not TryHexToByte(sr, r) then Exit;

    sg := Copy(s, 3, 2);
    if not TryHexToByte(sg, g) then Exit;

    sb := Copy(s, 5, 2);
    if not TryHexToByte(sb, b) then Exit;

    Color := RGB(r, g, b);
    Result := True;
  end;

end;

function TryStrToByte(s: string; var bt: Byte): Boolean;
var
  x: integer;
begin
  Result := False;
  if not TryStrToInt(s, x) then Exit;
  if not (x in [0..255]) then Exit;
  bt := Byte(x);
  Result := True;
end;

function RemoveAll(const Text, ToRemove: string; IgnoreCase: Boolean = False): string;
var
  rf: TReplaceFlags;
begin
  rf := [rfReplaceAll];
  if IgnoreCase then rf := rf + [rfIgnoreCase];
  Result := StringReplace(Text, ToRemove, '', rf);
end;

function RGB3(const bt: Byte): TColor;
begin
  Result := RGB(bt, bt, bt);
end;

function TryRgbStrToColor(s: string; out Color: TColor): Boolean;
var
  s2: string;
  r, g, b: Byte;
  xp: integer;
begin
  r := 0; g := 0; b := 0;
  Result := False;
  //s := StringReplace(s, '   ', ' ', [rfReplaceAll]);
  //s := StringReplace(s, '  ', ' ', [rfReplaceAll]);
  s := StringReplace(s, ' ', ',', [rfReplaceAll]);
  //s := StringReplace(s, ',,', ',', [rfReplaceAll]);
  s := RemoveAll(s, 'rgb', True);
  s := RemoveAll(s, '(');
  s := RemoveAll(s, ')');

  xp := Pos(',', s);
  if xp > 0 then
  begin
    s2 := Copy(s, 1, xp - 1);
    if not TryStrToByte(s2, r) then Exit;
    s := Copy(s, xp + 1, Length(s));

    xp := Pos(',', s);
    if xp > 0 then
    begin
      s2 := Copy(s, 1, xp - 1);
      if not TryStrToByte(s2, g) then Exit;

      s := Copy(s, xp + 1, Length(s));
      if not TryStrToByte(s, b) then Exit;

      Color := RGB(r,g,b);
      Result := True;
    end;
  end
  else
    if TryStrToByte(s, r) then
    begin
      Color := RGB3(r);
      Result := True;
      Exit;
    end;

end;

{$endregion}

function ParamToColor(A: String): TColor;
begin
  if A.StartsWith('#') then if TryHtmlStrToColor(A, Result) then Exit;
  if (A.StartsWith('rgb', True)) or (Pos(',', A) > 0) or (Pos(' ', A) > 0) then if TryRgbStrToColor(A, Result) then Exit;

  if A.StartsWith('$') then Insert('00', A, 2);
  {At HTML, is used Hexadecimal color code with 6 digits, the same used at
  this component. However the Delphi works with 8 digits, but the first two
  digits are always "00"}

  try
    Result := StringToColor(A);
  except
    Result := clNone;
  end;
end;

{$region ' ----------------------- TBuilder (part 2) / TListStack --------------------- '}
procedure TBuilder.AddToken(aKind: TTokenKind; aTagClose: Boolean = False; aText: String = ''; aValue: Integer = 0);
var T: TToken;
begin
  T := TToken.Create;
  T.Kind := aKind;
  T.TagClose := aTagClose;
  T.Text := aText;
  T.Value := aValue;
  L.Add(T);
end;

  {$region ' --- TBuilder.ProcessTag --- '}
function TBuilder.ProcessTag(Tag: String): Boolean;
var TOff, TOn, HasPar: Boolean;
    Kind: TTokenKind;
    Value: Integer;
    A, Par: String;
    I: Integer;
begin
  //Result=True means valid tag
  Result := False;
  A := Tag;

  TOff := False;
  if A.StartsWith('/') then //closing tag
  begin
      TOff := True;
      Delete(A, 1, 1);
  end;
  TOn := not TOff;

  HasPar := False;
  Par := '';
  I := Pos(':', A); //find parameter
  if I>0 then //has parameter
  begin
      HasPar := True;
      Par := A;
      Delete(Par, 1, I);
      //Par := Copy(A, I+1, Length(A)-I);
      A := Copy(A, 1, I-1);
  end;

  if TOff and HasPar then Exit; //tag closing with parameter

  Value := 0;
  A := UpperCase(A);
  if (A='BR') and TOn and not HasPar then //LINE BREAK
  begin
      AddToken(ttBreak);
      Result := True;
  end else
  if (A='B') and not HasPar then //BOLD
  begin
      AddToken(ttBold, TOff);
      Result := True;
  end else
  if (A='I') and not HasPar then //ITALIC
  begin
      AddToken(ttItalic, TOff);
      Result := True;
  end else
  if (A='U') and not HasPar then //UNDERLINE
  begin
      AddToken(ttUnderline, TOff);
      Result := True;
  end else
  if (A='S') and not HasPar then //STRIKEOUT
  begin
      AddToken(ttStrike, TOff);
      Result := True;
  end else
  if A='FN' then //FONT NAME
  begin
      if TOn and (Par='') then Exit;
      AddToken(ttFontName, TOff, Par);
      Result := True;
  end else
  if A='FS' then //FONT SIZE
  begin
      if TOn then
      begin
        Value := StrToIntDef(Par, 0);
        if Value<=0 then Exit;
      end;
      AddToken(ttFontSize, TOff, '', Value);
      Result := True;
  end else
  if A='FC' then //FONT COLOR
  begin
      if TOn then
      begin
        Value := ParamToColor(Par);
        if Value=clNone then Exit;
      end;
      AddToken(ttFontColor, TOff, '', Value);
      Result := True;
  end else
  if A='BC' then //BACKGROUND COLOR
  begin
      if TOn then
      begin
        Value := ParamToColor(Par);
        if Value=clNone then Exit;
      end;
      AddToken(ttBackColor, TOff, '', Value);
      Result := True;
  end else
  if A='A' then //LINK
  begin
      if TOn and HasPar and (Par='') then Exit;
      AddToken(ttLink, TOff, Par);
      Result := True;
  end else
  if (A='L') and not HasPar then //ALIGN LEFT
  begin
      AddToken(ttAlignLeft, TOff);
      Result := True;
  end else
  if (A='C') and not HasPar then //ALIGN CENTER
  begin
      AddToken(ttAlignCenter, TOff);
      Result := True;
  end else
  if (A='R') and not HasPar then //ALIGN RIGHT
  begin
      AddToken(ttAlignRight, TOff);
      Result := True;
  end else
  if ((A='T') or (A='TF')) and TOn then //TAB
  begin
      Value := StrToIntDef(Par, 0);
      if Value<=0 then Exit;
      Kind := ttTab;
      if A='TF' then Kind := ttTabF;
      AddToken(Kind, TOff, '', Value);
      Result := True;
  end
  // ------------------- jp ------------------------
  else if (A = 'HR') then
  begin
    Value := StrToIntDef(Par, 0);
    AddToken(ttHorizontalLine, TOff, '', Value);
    Result := True;
  end
  else if (A = 'H1') then
  begin
    AddToken(ttHeader1, TOff);
    Result := True;
  end
  else if (A = 'H2') then
  begin
    AddToken(ttHeader2, TOff);
    Result := True;
  end
  else if (A = 'H3') then
  begin
    AddToken(ttHeader3, TOff);
    Result := True;
  end
  else if (A = 'LC') then
  begin
    Value := ParamToColor(Par);
    if Value = clNone then Exit;
    AddToken(ttLineColor, TOff, '', Value);
    Result := True;
  end
  else if (A = 'LI') then
  begin
    AddToken(ttListItem, TOff, Par);
    Result := True;
  end
  else if (A = 'LI2') then
  begin
    AddToken(ttListItem2, TOff, Par);
    Result := True;
  end
  else if (A = 'IMG') then
  begin
    if TOn and HasPar and (Par = '') then Exit;
    AddToken(ttImage, TOff, Par, Value);
    Result := True;
  end
  else if (A = 'BBC') then
  begin
    Value := ParamToColor(Par);
    if Value = clNone then Exit;
    AddToken(ttBodyBackgroundColor, TOff, '', Value);
    Result := True;
  end
  else if (A = 'SUB') then
  begin
    AddToken(ttSubscript, TOff);
    Result := True;
  end
  else if (A = 'SUP') then
  begin
    AddToken(ttSuperscript, TOff);
    Result := True;
  end;
end;
  {$endregion TBuilder.ProcessTag}

  {$region ' --- TBuilder.BuildTokens --- '}
procedure TBuilder.BuildTokens;
var Text, A: String;
    CharIni: Char;
    I, Jump: Integer;
begin
  Text := StringReplace(Lb.FText, #13#10, '<BR>', [rfReplaceAll]);
  while Text<>'' do
  begin
    A := Text;
    CharIni := A[1];

    if CharIni = '<' then //starts with tag opening
    begin
      Delete(A, 1, 1);
      I := Pos('>', A); //find tag closing
      if I>0 then
      begin
          A := Copy(A, 1, I-1);
          if not ProcessTag(A) then AddToken(ttInvalid);
          Jump := 1+Length(A)+1;
      end else
      begin
          //losted tag opening
          AddToken(ttInvalid);
          Jump := 1;
      end;
    end else
    if CharIni = '>' then
    begin
      //losted tag closing
      AddToken(ttInvalid);
      Jump := 1;
    end else
    if CharIni = ' ' then //space
    begin
      AddToken(ttSpace, False, ' ');
      Jump := 1;
    end else
    if CharInSet(CharIni, ['/','\']) then
    begin
      //this is to break line when using paths
      AddToken(ttText, False, CharIni);
      Jump := 1;
    end else
    begin //all the rest is text
      I := A.IndexOfAny([' ','<','>','/','\'])+1; //warning: 0-based function!!!
      if I=0 then I := Length(A)+1;

      Dec(I);
      A := Copy(A, 1, I);
      AddToken(ttText, False, ReplaceHtmlEntities(A));
      Jump := I;
    end;

    Delete(Text, 1, Jump);
  end;
end;
  {$endregion TBuilder.BuildTokens}

type
  TListStack<T> = class(TList<T>)
    procedure AddOrDel(Token: TToken; const XValue: T);
  end;

procedure TListStack<T>.AddOrDel(Token: TToken; const XValue: T);
begin
  if Token.TagClose then
  begin
    if Count>1 then
      Delete(Count-1);
  end else
    Add(XValue);
end;

  {$region ' --- TBuilder.BuildWords --- '}
procedure TBuilder.BuildWords;
var C: TCanvas;

    X, Y, HighW, HighH, LineCount: Integer;
    LastTabF: Boolean; //last tabulation was TabF (with break align)
    LastTabF_X: Integer;
    dy, dx, xs, xfs: integer;
    LastLI, LastLI2: Boolean;
    bSubscript, bSuperscript, bHeader1, bHeader2, bHeader3: Boolean;

//  procedure DoLineBreak;
//  begin
//    if HighH=0 then HighH := C.TextHeight(' '); //line without content
//    Inc(Y, HighH); //inc biggest height of the line
//    HighH := 0; //clear line height
//
//    if X>HighW then HighW := X; //store width of biggest line
//    X := 0; //carriage return :)
//    if LastTabF then X := LastTabF_X; //last line breaks with TabF
//
//    LGroupBound.Add(Lb.Width); //add line bound to use in group align
//    Inc(LineCount);
//  end;
  procedure DoLineBreak;
  begin
    if HighH = 0 then HighH := C.TextHeight(' '); //line without content
    Inc(Y, HighH + Self.Lb.ExtraLineSpacing); //inc biggest height of the line
    HighH := 0; //clear line height

    if X > HighW then HighW := X; //store width of biggest line
    X := 0; //carriage return :)
    if LastTabF then X := LastTabF_X; //last line breaks with TabF

    if LastLI then X := Lb.TagLIParams.Margin;
    if LastLI2 then X := Lb.TagLI2Params.Margin;

    LGroupBound.Add(Lb.FBitmapWidth); //add line bound to use in group align
    Inc(LineCount);
  end;

var
  T: TToken;
  I: Integer;

  Ex: TSize; FS: TFontStyles;
  PreWidth: Integer;

  LinkOn: Boolean;
  LinkID: Integer;

  BackColor: TColor;
  Align: TAlignment;

  LBold: TListStack<Boolean>;
  LItalic: TListStack<Boolean>;
  LUnderline: TListStack<Boolean>;
  LStrike: TListStack<Boolean>;
  LFontName: TListStack<String>;
  LFontSize: TListStack<Integer>;
  LFontColor: TListStack<TColor>;
  LBackColor: TListStack<TColor>;
  LAlign: TListStack<TAlignment>;

  LinkData: TDHLinkData;
  {$IFDEF FPC}b: Boolean;{$ENDIF}

  procedure SetLastParams;
  begin
    C.Font.Color := LFontColor.Last;
    C.Font.Size := LFontSize.Last;
    C.Font.Name := LFontName.Last;
    BackColor := LBackColor.Last;
    Align := LAlign.Last;
    FS := [];
    if LBold.Last then Include(FS, fsBold);
    if LItalic.Last then Include(FS, fsItalic);
    if LUnderline.Last then Include(FS, fsUnderline);
    if LStrike.Last then Include(FS, fsStrikeOut);
    C.Font.Style := FS;
  end;
begin
  C := Lb.Canvas;
  C.Font.Assign(Lb.Font);

  BackColor := clNone;
  Align := taLeftJustify;

  LBold := TListStack<Boolean>.Create;
  LItalic := TListStack<Boolean>.Create;
  LUnderline := TListStack<Boolean>.Create;
  LStrike := TListStack<Boolean>.Create;
  LFontName := TListStack<String>.Create;
  LFontSize := TListStack<Integer>.Create;
  LFontColor := TListStack<TColor>.Create;
  LBackColor := TListStack<TColor>.Create;
  LAlign := TListStack<TAlignment>.Create;

  try
    {$IFDEF FPC}
    // LBold.Add(fsBold in C.Font.Style); --> FPC: Internal error 2011010304 // Why?
    b := fsBold in C.Font.Style;
    LBold.Add(b);
    b := fsItalic in C.Font.Style;
    LItalic.Add(b);
    b := fsUnderline in C.Font.Style;
    LUnderline.Add(b);
    b := fsStrikeOut in C.Font.Style;
    LStrike.Add(b);
    {$ELSE}
    LBold.Add(fsBold in C.Font.Style);
    LItalic.Add(fsItalic in C.Font.Style);
    LUnderline.Add(fsUnderline in C.Font.Style);
    LStrike.Add(fsStrikeOut in C.Font.Style);
    {$ENDIF}
    LFontName.Add(C.Font.Name);
    LFontSize.Add(C.Font.Size);
    LFontColor.Add(C.Font.Color);
    LBackColor.Add(BackColor);
    LAlign.Add(Align);

    X := 0;
    Y := 0;

    HighW := 0;
    HighH := 0;

    LineCount := 0;

    LastTabF := False;
    LastTabF_X := 0;

    LinkOn := False;
    LinkID := -1;

    LastLI := False;
    LastLI2 := False;
    bSubscript := False;
    bSuperscript := False;
    bHeader1 := False;


    for I := 0 to L.Count-1 do
    begin

      T := L[I];

      case T.Kind of

        // ------------------------ <HR> ---------------------------
        ttHorizontalLine:
          begin
            if T.Value = 0 then T.Value := Lb.FBitmapWidth
            else
            begin
              if T.Value < 0 then T.Value := Lb.FBitmapWidth + T.Value;
              if Lb.InternalMargins.Enabled then T.Value := T.Value - Lb.InternalMargins.Right + Lb.InternalMargins.Left;
            end;
            if LastTabF then dx := LastTabF_X else dx := X;
            //T.Value := T.Value - dx;

            dy := C.TextHeight('1');

            Lb.LWords.Add(
              {$IFDEF FPC}Types.{$ENDIF}Rect(
                dx  , Y,
                T.Value, Y + dy
              ),
              T.Text, LGroupBound.Count, Align, C.Font, BackColor, LinkOn, LinkID, False, 0, TAG_ID_HR
            );

            LGroupBound.Add(Lb.FBitmapWidth);
            Inc(LineCount);
            Continue;
          end;

        // -------------------------- <LC> - line color ---------------------------
        ttLineColor:
          begin
            Ex := C.TextExtent('1');
            Lb.LWords.Add({$IFDEF FPC}Types.{$ENDIF}Rect(X, Y, 0, Y + Ex.Height),
              T.Text, LGroupBound.Count, Align, C.Font, BackColor, LinkOn, LinkID, False, T.Value, TAG_ID_LC);
            Continue;
          end;

        // -------------------------- <BBC> - body background color ---------------------------
        ttBodyBackgroundColor:
          begin
            Lb.LWords.Add({$IFDEF FPC}Types.{$ENDIF}Rect(X, Y, 0, 0),
              T.Text, LGroupBound.Count, Align, C.Font, BackColor, LinkOn, LinkID, False, T.Value, TAG_ID_BBC);
          end;

        // ----------------------------- <IMG> ------------------------------
        ttImage:
          begin
            Lb.LWords.Add({$IFDEF FPC}Types.{$ENDIF}Rect(X, Y, 0, Y + Ex.Height),
              T.Text, LGroupBound.Count, Align, C.Font, BackColor, LinkOn, LinkID, False, T.Value, TAG_ID_IMG);
          end;

        // -------------------------- <SUB> - subscript ---------------------------
        ttSubscript:
          begin
            bSubscript := not T.TagClose;
            if T.TagClose then
            begin
              if bHeader1 then C.Font.Assign(Lb.TagH1Params.Font)
              else if bHeader2 then C.Font.Assign(Lb.TagH2Params.Font)
              else if bHeader3 then C.Font.Assign(Lb.TagH3Params.Font)
              else C.Font.Size := LFontSize.Last;
            end;
          end;

        // -------------------------- <SUP> - superscript ---------------------------
        ttSuperscript:
          begin
            bSuperscript := not T.TagClose;
            if T.TagClose then
            begin
              if bHeader1 then C.Font.Assign(Lb.TagH1Params.Font)
              else if bHeader2 then C.Font.Assign(Lb.TagH2Params.Font)
              else if bHeader3 then C.Font.Assign(Lb.TagH3Params.Font)
              else C.Font.Size := LFontSize.Last;
            end;
          end;

        // -------------------------- <H1> ----------------------------
        ttHeader1:
          begin
            bHeader1 := not T.TagClose;
            if T.TagClose then SetLastParams
            else
            begin
              C.Font.Assign(Lb.TagH1Params.Font);
              Ex := C.TextExtent(T.Text);
              Align := Lb.TagH1Params.Alignment;
              if not Lb.TagH1Params.Transparent then BackColor := Lb.TagH1Params.BackgroundColor;

              Lb.LWords.Add({$IFDEF FPC}Types.{$ENDIF}Rect(X, Y, X + Ex.Width, Y + Ex.Height),
                T.Text, LGroupBound.Count, Align, C.Font, BackColor, LinkOn, LinkID, False);
            end;
          end;

        // -------------------------- <H2> ----------------------------
        ttHeader2:
          begin
            bHeader2 := not T.TagClose;
            if T.TagClose then SetLastParams
            else
            begin
              C.Font.Assign(Lb.TagH2Params.Font);
              Ex := C.TextExtent(T.Text);
              Align := Lb.TagH2Params.Alignment;
              if not Lb.TagH2Params.Transparent then BackColor := Lb.TagH2Params.BackgroundColor;

              Lb.LWords.Add({$IFDEF FPC}Types.{$ENDIF}Rect(X, Y, X+Ex.Width, Y+Ex.Height),
                T.Text, LGroupBound.Count, Align, C.Font, BackColor, LinkOn, LinkID, False);
            end;
          end;

        // -------------------------- <H3> ----------------------------
        ttHeader3:
          begin
            bHeader3 := not T.TagClose;
            if T.TagClose then SetLastParams
            else
            begin
              C.Font.Assign(Lb.TagH3Params.Font);
              Ex := C.TextExtent(T.Text);
              Align := Lb.TagH3Params.Alignment;
              if not Lb.TagH3Params.Transparent then BackColor := Lb.TagH3Params.BackgroundColor;

              Lb.LWords.Add({$IFDEF FPC}Types.{$ENDIF}Rect(X, Y, X+Ex.Width, Y+Ex.Height),
                T.Text, LGroupBound.Count, Align, C.Font, BackColor, LinkOn, LinkID, False);
            end;
          end;

        // -------------------------- <B> <I> <U> <S> ----------------------------
        ttBold, ttItalic, ttUnderline, ttStrike:
          begin
            case T.Kind of
              ttBold: LBold.AddOrDel(T, True);
              ttItalic: LItalic.AddOrDel(T, True);
              ttUnderline: LUnderline.AddOrDel(T, True);
              ttStrike: LStrike.AddOrDel(T, True);
            end;

            FS := [];
            if LBold.Last then Include(FS, fsBold);
            if LItalic.Last then Include(FS, fsItalic);
            if LUnderline.Last then Include(FS, fsUnderline);
            if LStrike.Last then Include(FS, fsStrikeOut);
            C.Font.Style := FS;
          end;

        // -------------------------- <FN> - font name ----------------------------
        ttFontName:
          begin
            LFontName.AddOrDel(T, T.Text);
            C.Font.Name := LFontName.Last;
          end;

        // -------------------------- <FS> - font size ----------------------------
        ttFontSize:
          begin
            LFontSize.AddOrDel(T, T.Value);
            C.Font.Size := LFontSize.Last;
          end;

        // -------------------------- <FC> - font color> ----------------------------
        ttFontColor:
          begin
            LFontColor.AddOrDel(T, T.Value);
            C.Font.Color := LFontColor.Last;
          end;

        // -------------------------- <BC> - text background color ----------------------------
        ttBackColor:
          begin
            LBackColor.AddOrDel(T, T.Value);
            BackColor := LBackColor.Last;
          end;

        // -------------------------- <C> <R> <L> - alignment ----------------------------
        ttAlignLeft, ttAlignCenter, ttAlignRight:
          begin
            case T.Kind of
              ttAlignLeft: Align := taLeftJustify;
              ttAlignCenter: Align := taCenter;
              ttAlignRight: Align := taRightJustify;
            end;
            LAlign.AddOrDel(T, Align);
            Align := LAlign.Last;
          end;

        // ------------------------------ T E X T / space / invalid ----------------------------------
        ttText, ttSpace, ttInvalid:
          begin

            dx := 0;
            dy := 0;

            // -------------- <SUB> - subscript -----------------
            if bSubscript then
            begin
              if bHeader1 then xfs := Lb.TagH1Params.Font.Size
              else if bHeader2 then xfs := Lb.TagH2Params.Font.Size
              else if bHeader3 then xfs := Lb.TagH3Params.Font.Size
              else xfs := C.Font.Size;

              // fix Y position
              case xfs of
                1..8: Dec(dy, 2);
                9: Dec(dy, 1);
                10: Dec(dy, 0);
                11..12: Dec(dy, -1);
                13..14: Dec(dy, 1);
                15..17: Dec(dy, -1);
                18: Dec(dy, 0);
                19: Dec(dy, 0);
                20: Dec(dy, 1);
                21..22: Dec(dy, 3);
                23..25: Dec(dy, 0);
              end;
              dy := dy + Lb.TagSUBParams.PosYDelta;

              // fix font size
              case xfs of
                1..8: xs := 1;
                9..11: xs := 2;
                12..14: xs := 3;
                15..18: xs := 4;
                19..22: xs := 5;
                23..29: xs := 7;
              else
                xs := 9;
              end;

              C.Font.Size := {LFontSize.Last} C.Font.Size - xs + Lb.TagSUBParams.FontSizeDelta;
              dy := (C.TextHeight('1') div 2) + dy;
            end
            //;//else C.Font.Size := LFontSize.Last;
            else if bSuperscript then
            begin
              if bHeader1 then xfs := Lb.TagH1Params.Font.Size
              else if bHeader2 then xfs := Lb.TagH2Params.Font.Size
              else if bHeader3 then xfs := Lb.TagH3Params.Font.Size
              else xfs := C.Font.Size;

              // fix Y position
              case xfs of
                1..8: Dec(dy, 2);
                9: Dec(dy, 3);
                10: Dec(dy, 5);
//                11: Dec(dy, 5);
//                12: Dec(dy, 6);
//                13: Dec(dy, 7);
//                14: Dec(dy, 7);
//                15..17: Dec(dy, 9);
//                18: Dec(dy, 10);
//                19: Dec(dy, 11);
//                20: Dec(dy, 12);
//                21..22: Dec(dy, 13);
//                23..24: Dec(dy, 14);
              else
                Dec(dy, (xfs div 2) - 1);
              end;
              dy := dy + Lb.TagSUPParams.PosYDelta;

              // fix font size
              case xfs of
                1..8: xs := 1;
                9: xs := 2;
                10..11: xs := 3;
                12..13: xs := 4;
                14..15: xs := 5;
                16..17: xs := 6;
//                18..19: xs := 7;
//                20..21: xs := 8;
//                22..23: xs := 9;
              else
                xs := (xfs div 2) - 4;
              end;

              C.Font.Size := {LFontSize.Last} C.Font.Size - xs + Lb.TagSUPParams.FontSizeDelta;
              dy := -(C.TextHeight('1') div 2) - dy;
            end;



            case T.Kind of
              ttSpace: T.Text := ' ';
              ttInvalid: T.Text := '<?>';
            end;

            Ex := C.TextExtent(T.Text);
            Ex.cx := Ex.cx + Self.Lb.ExtraWordSpacing; // jp: extra word spacing
            PreWidth := X + Ex.Width;

            if
              ((Lb.FAutoWidth) and (Lb.FMaxWidth > 0) and (PreWidth > Lb.FMaxWidth)) or
              ((not Lb.FAutoWidth) and (PreWidth > Lb.FBitmapWidth)) then
            begin
              //clear last word on line break when is space to not comsume pixels at end of line
              if Lb.LWords.Count > 0 then
                if Lb.LWords.Last.Space then
                begin
                  Dec(X, Lb.LWords.Last.Rect.Width);
                  Lb.LWords.Delete(Lb.LWords.Count - 1);
                end;

              DoLineBreak;

              if T.Kind=ttSpace then Continue;
            end;

            if Ex.Height > HighH then HighH := Ex.Height; //biggest height of the line

            Lb.LWords.Add({$IFDEF FPC}Types.{$ENDIF}Rect(
                X, Y,
                X + Ex.Width + dx, Y + Ex.Height + dy
              ),
              T.Text, LGroupBound.Count, Align, C.Font, BackColor, LinkOn, LinkID, T.Kind=ttSpace);

            Inc(X, Ex.Width);

          end;

        // -------------------------- <A> - links ----------------------------
        ttLink:
          begin
            if T.TagClose then
            begin
              if LinkID<>-1 then
                Lb.LLinkData[LinkID].FText := L.GetLinkText(I); //set link display text on the link data object

              LinkOn := False;
              LinkID := -1;
            end else
            begin
              LinkData := TDHLinkData.Create;
              LinkData.FTarget := T.Text;

              LinkOn := True;
              LinkID := Lb.LLinkData.Add(LinkData); //add target of the link on list
            end;
          end;

        // -------------------------- <T> <TF> - indentation ----------------------------
        ttTab, ttTabF:
          begin
            X := T.Value; //cursor position

            LastTabF := T.Kind=ttTabF;
            LastTabF_X := X;

            LGroupBound.Add(X); //add bound on last group to use at text align
          end;

        // -------------------- <LI> - list item (1st level) --------------------
        ttListItem:
          begin
            X := Lb.TagLIParams.Margin;
            Ex := C.TextExtent(T.Text);
            Lb.LWords.Add(
              {$IFDEF FPC}Types.{$ENDIF}Rect(
                X, Y,
                X + Ex.Width, Y + Ex.Height
              ),
              T.Text, LGroupBound.Count, Align, C.Font, BackColor, LinkOn, LinkID, False, T.Value, TAG_ID_LI);
            LastLI := True;
            LGroupBound.Add(X);
          end;

        // -------------------- <LI2> - list item (2nd level) --------------------
        ttListItem2:
          begin
            X := Lb.TagLI2Params.Margin;
            Ex := C.TextExtent(T.Text);
            Lb.LWords.Add(
              {$IFDEF FPC}Types.{$ENDIF}Rect(
                X, Y,
                X + Ex.Width, Y + Ex.Height
              ),
              T.Text, LGroupBound.Count, Align, C.Font, BackColor, LinkOn, LinkID, False, T.Value, TAG_ID_LI2);
            LastLI2 := True;
            LGroupBound.Add(X);
          end;



        // -------------------------- <BR> ----------------------------
        ttBreak:
          begin
            LastTabF := False; //clear TabF
            LastLI := False;
            LastLI2 := False;
            DoLineBreak;
          end;

      end; // case T.Kind

    end; // for I


  finally
    LBold.Free;
    LItalic.Free;
    LUnderline.Free;
    LStrike.Free;
    LFontName.Free;
    LFontSize.Free;
    LFontColor.Free;
    LBackColor.Free;
    LAlign.Free;
  end;

  if Lb.LWords.Count>0 then DoLineBreak;
  CalcWidth := HighW;
  if Lb.InternalMargins.Enabled then CalcWidth := CalcWidth + Lb.InternalMargins.Left + Lb.InternalMargins.Right;
  CalcHeight := Y;
  Lb.FLines := LineCount;
end;
  {$endregion TBuilder.BuildWords}

procedure TBuilder.CheckAligns;
var W: TDHWord;
    LW: array of Integer;
    Group, I, SumW, Offset: Integer;
begin
  SetLength(LW, LGroupBound.Count);

  Group := -1;
  SumW := 0;

  for I := 0 to Lb.LWords.Count-1 do
  begin
    W := Lb.LWords[I];

    if W.Group<>Group then //enter new group
    begin
      if I>0 then
        LW[Group] := SumW; //add last group width sum

      Group := W.Group;
      SumW := W.Rect.Left; //where first group starts
    end;

    Inc(SumW, W.Rect.Width);
    if I=Lb.LWords.Count-1 then LW[Group] := SumW;
  end;

  for W in Lb.LWords do
    if W.Align in [taCenter, taRightJustify] then
    begin
      Offset := LGroupBound[W.Group] - LW[W.Group];
      if W.Align=taCenter then Offset := Offset div 2;

      W.Rect.Offset(Offset, 0);
    end;
end;
{$endregion TBuilder}


{$REGION ' ------------ TDHStyleLinkProp ------------ '}
constructor TDHStyleLinkProp.Create(xLb: TDzHTMLText; xKind: TDHKindStyleLinkProp);
begin
  inherited Create;

  Lb := xLb;
  Kind := xKind;

  FFontColor := GetDefaultFontColor;
  FBackColor := clNone;
end;

function TDHStyleLinkProp.GetOwner: TPersistent;
begin
  Result := Lb;
end;

function TDHStyleLinkProp.GetDefaultFontColor: TColor;
begin
  Result := clNone;
  case Kind of
    tslpNormal: Result := clBlue;
    tslpHover: Result := clRed;
  end;
end;

function TDHStyleLinkProp.GetStoredFontColor: Boolean;
begin
  Result := FFontColor<>GetDefaultFontColor;
end;

procedure TDHStyleLinkProp.SetFontColor(const Value: TColor);
begin
  if Value <> FFontColor then
  begin
    FFontColor := Value;

    Lb.BuildAndPaint;
  end;
end;

procedure TDHStyleLinkProp.SetBackColor(const Value: TColor);
begin
  if Value <> FBackColor then
  begin
    FBackColor := Value;

    Lb.BuildAndPaint;
  end;
end;

procedure TDHStyleLinkProp.SetUnderline(const Value: Boolean);
begin
  if Value <> FUnderline then
  begin
    FUnderline := Value;

    Lb.BuildAndPaint;
  end;
end;

procedure TDHStyleLinkProp.SetPropsToCanvas(C: TCanvas);
begin
  if FFontColor<>clNone then C.Font.Color := FFontColor;
  if FBackColor<>clNone then C.Brush.Color := FBackColor;
  if FUnderline then C.Font.Style := C.Font.Style + [fsUnderline];
end;

procedure TDHStyleLinkProp.Assign(Source: TPersistent);
begin
  if Source is TDHStyleLinkProp then
  begin
    Self.FFontColor := TDHStyleLinkProp(Source).FFontColor;
    Self.FBackColor := TDHStyleLinkProp(Source).FBackColor;
    Self.FUnderline := TDHStyleLinkProp(Source).FUnderline;
  end else
    inherited;
end;

function TDHStyleLinkProp.GetStored: Boolean;
begin
  Result := GetStoredFontColor
         or FUnderline
         or (FBackColor<>clNone);
end;

procedure TDzHTMLText.SetStyleLink(const Index: Integer;
  const Value: TDHStyleLinkProp);
begin
  case Index of
    1: FStyleLinkNormal.Assign(Value);
    2: FStyleLinkHover.Assign(Value);
  end;
end;

function TDzHTMLText.GetStoredStyleLink(const Index: Integer): Boolean;
begin
  Result := False;
  case Index of
    1: Result := FStyleLinkNormal.GetStored;
    2: Result := FStyleLinkHover.GetStored;
  end;
end;
{$endregion TDHStyleLinkProp}


{$region ' ------------ TListToken -------- '}
function TListToken.GetLinkText(IEnd: Integer): String;
var I: Integer;
  T: TToken;
begin
  //returns the link display text where IEnd is Link Close tag Token on the list
  //so, it will start from the end until find the Link Open tag.

  Result := '';
  for I := IEnd-1 downto 0 do
  begin
    T := Items[I];
    if T.Kind = ttLink then Break; //should be open tag

    if T.Kind in [ttText, ttSpace] then
      Result := T.Text + Result;
  end;
end;
{$endregion TListToken}


{$region ' ------------- TDHTagHRParams ------------ '}
constructor TDHTagHRParams.Create;
begin
  inherited Create;
  FStyle := psSolid;
  FLineHeight := 1;
  FOnChange := nil;
end;

procedure TDHTagHRParams.SetLineHeight(const Value: integer);
begin
  if FLineHeight = Value then Exit;
  FLineHeight := Value;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TDHTagHRParams.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TDHTagHRParams.SetStyle(const Value: TPenStyle);
begin
  if FStyle = Value then Exit;
  FStyle := Value;
  if Assigned(OnChange) then OnChange(Self);
end;
{$endregion TDHTagHRParams}


{$region ' ------------ TDHTagHeaderParams + H1, H2, H3 -------------- '}
constructor TDHTagHeaderParams.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FFont.OnChange := OnChange;
  FFont.Style := [fsBold];
  FBackgroundColor := clBtnFace;
  FAlignment := taLeftJustify;
  FTransparent := True;
  FOnChange := nil;
end;

destructor TDHTagHeaderParams.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TDHTagHeaderParams.SetAlignment(const Value: TAlignment);
begin
  if FAlignment = Value then Exit;
  FAlignment := Value;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TDHTagHeaderParams.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor = Value then Exit;
  FBackgroundColor := Value;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TDHTagHeaderParams.SetFont(const Value: TFont);
begin
  FFont := Value;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TDHTagHeaderParams.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
  FFont.OnChange := Value;
end;

procedure TDHTagHeaderParams.SetTransparent(const Value: Boolean);
begin
  if FTransparent = Value then Exit;
  FTransparent := Value;
  if Assigned(OnChange) then OnChange(Self);
end;


constructor TDHTagH1Params.Create;
begin
  inherited Create;
  FFont.Size := 18;
  FFont.Name := 'Arial';
end;


constructor TDHTagH2Params.Create;
begin
  inherited Create;
  FFont.Size := 15;
  FFont.Name := 'Arial';
end;


constructor TDHTagH3Params.Create;
begin
  inherited Create;
  FFont.Size := 12;
  FFont.Name := 'Arial';
end;
{$endregion TDHTagHeaderParams + H1, H2, H3}


{$region ' -------------- TDHInternalMargins ------------- ' }
constructor TDHInternalMargins.Create;
begin
  inherited Create;
  FLeft := 8;
  FRight := 8;
  FEnabled := False;
  FOnChange := nil;
end;

procedure TDHInternalMargins.SetEnabled(const Value: Boolean);
begin
  if FEnabled = Value then Exit;
  FEnabled := Value;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TDHInternalMargins.SetLeft(const Value: integer);
begin
  if FLeft = Value then Exit;
  FLeft := Value;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TDHInternalMargins.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TDHInternalMargins.SetRight(const Value: integer);
begin
  if FRight = Value then Exit;
  FRight := Value;
  if Assigned(OnChange) then OnChange(Self);
end;
{$endregion TDHInternalMargins}


{$region ' --------------- TDHTagLIParams / TDHTagLI1Params / TDHTagLI1Params --------------- '}

constructor TDHTagLIParams.Create;
begin
  inherited Create;
  FBulletType := btBullet;
  FCustomString := '';
  FSpacing := 5;
end;

procedure TDHTagLIParams.SetBulletType(const Value: TDHBulletType);
begin
  if FBulletType = Value then Exit;
  FBulletType := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TDHTagLIParams.SetCustomString(const Value: string);
begin
  if FCustomString = Value then Exit;
  FCustomString := Value;
  if (FBulletType = btCustomString) and (Assigned(FOnChange)) then FOnChange(Self);
end;

procedure TDHTagLIParams.SetMargin(const Value: integer);
begin
  if FMargin = Value then Exit;
  FMargin := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TDHTagLIParams.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TDHTagLIParams.SetSpacing(const Value: Byte);
begin
  if FSpacing = Value then Exit;
  FSpacing := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;


constructor TDHTagLI1Params.Create;
begin
  inherited Create;
  FBulletType := btBullet;
  FMargin := 20;
  FSpacing := 5;
end;

constructor TDHTagLI2Params.Create;
begin
  inherited Create;
  FBulletType := btDash;
  FMargin := 35;
  FSpacing := 5;
end;
{$endregion TDHTagLIParams / TDHTagLI1Params / TDHTagLI1Params}


{$region ' ---------------- TDHBorder ---------------- '}

constructor TDHBorder.Create;
begin
  inherited Create;
  FPen := TPen.Create;
  FPen.OnChange := FOnChange;
  FVisible := False;
end;

destructor TDHBorder.Destroy;
begin
  FPen.Free;
  inherited;
end;

procedure TDHBorder.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
  FPen.OnChange := Value;
end;

procedure TDHBorder.SetPen(const Value: TPen);
begin
  FPen := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TDHBorder.SetVisible(const Value: Boolean);
begin
  if FVisible = Value then Exit;
  FVisible := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;
{$endregion TDHBorder}


{$region ' -------------------- TDHTagSubParams --------------- '}

constructor TDHTagSubParams.Create;
begin
  inherited Create;
  FPosYDelta := 0;
  FFontSizeDelta := 0;
end;

procedure TDHTagSubParams.SetFontSizeDelta(const Value: ShortInt);
begin
  if FFontSizeDelta = Value then Exit;
  FFontSizeDelta := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TDHTagSubParams.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TDHTagSubParams.SetPosYDelta(const Value: ShortInt);
begin
  if FPosYDelta = Value then Exit;
  FPosYDelta := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;
{$endregion TDHTagSubParams}


{$region ' -------------------- TDHTagSupParams --------------- '}

constructor TDHTagSupParams.Create;
begin
  inherited Create;
  FPosYDelta := 0;
  FFontSizeDelta := 0;
end;

procedure TDHTagSupParams.SetFontSizeDelta(const Value: ShortInt);
begin
  if FFontSizeDelta = Value then Exit;
  FFontSizeDelta := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TDHTagSupParams.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TDHTagSupParams.SetPosYDelta(const Value: ShortInt);
begin
  if FPosYDelta = Value then Exit;
  FPosYDelta := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;
{$endregion TDHTagSupParams}

end.
