unit DzPngCollection;

{
  Jacek Pazera
  http://www.pazera-software.com
  https://github.com/jackdp
  Last mod: 2019.05.22
}

{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}

interface

uses
  {$IFDEF FPC}
  SysUtils, Classes, {Types, UITypes,} Graphics;
  {$ELSE}
  //Winapi.Messages, Winapi.Windows,
  System.SysUtils, System.Classes, System.Types, System.UITypes, Vcl.Graphics, Vcl.Imaging.PngImage;
  {$ENDIF}


type
  {$IFDEF FPC}
  TPngImage = TPortableNetworkGraphic;
  {$ENDIF}

  TDzPngCollectionItem = class;
  TDzPngCollectionItems = class;

  {$region ' --- TDzPngCollection --- '}
  TDzPngCollection = class(TComponent)
  private
    FItems: TDzPngCollectionItems;
    procedure EnableOrDisableAll(const Enable: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure EnableAll;
    procedure DisableAll;
    function PngIndex(const PngName: string; IgnoreCase: Boolean = False): integer;
    function Count: integer;
    function IsValidIndex(const Index: integer): Boolean;
    function AddPngImageFromFile(const FileName: string): integer; // Returns the index of the added item
    function AddPngImage(Png: TPngImage; ImageName: string = ''; Description: string = ''; Enabled: Boolean = True; ATag: integer = 0): integer;
    function GetPngImage(const Index: integer): TPngImage;
    function GetPngImageByName(const PngName: string; IgnoreCase: Boolean = True): TPngImage; // Returns the first PngImage with the given name
    function ReportStr: string; // for debug
  published
    property Items: TDzPngCollectionItems read FItems write FItems;
  end;
  {$endregion TDzPngCollection}


  {$region ' --- TDzPngCollectionItems --- '}
  TDzPngCollectionItems = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TDzPngCollectionItem;
    procedure SetItem(Index: Integer; const Value: TDzPngCollectionItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    function IsValidIndex(const Index: integer): Boolean;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TDzPngCollectionItem; //reintroduce;
    procedure Assign(Source: TPersistent); override;
    function Insert(Index: Integer): TDzPngCollectionItem;
    property Items[index: Integer]: TDzPngCollectionItem read GetItem write SetItem; default;
  end;
  {$endregion TDzPngCollectionItems}


  {$region ' --- TDzPngCollectionItem --- '}
  TDzPngCollectionItem = class(TCollectionItem)
  private
    FName: string;
    FDescription: string;
    FPngImage: TPngImage;
    FTag: integer;
    FEnabled: Boolean;
    procedure SetPngImage(const Value: TPngImage);
    function GetWidth: integer;
    function GetHeight: integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); overload; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property PngImage: TPngImage read FPngImage write SetPngImage;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property Tag: integer read FTag write FTag default 0;
    property Enabled: Boolean read FEnabled write FEnabled default True;
  end;
  {$endregion TDzPngCollectionItem}


//procedure Register;


implementation


//procedure Register;
//begin
//  RegisterComponents('Digao', [TDzPngCollection]);
//end;


{$region ' --------------------- TDzPngCollection ----------------------- '}
constructor TDzPngCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TDzPngCollectionItems.Create(Self);
end;

destructor TDzPngCollection.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TDzPngCollection.Clear;
begin
  FItems.Clear;
end;

function TDzPngCollection.Count: integer;
begin
  Result := FItems.Count;
end;

procedure TDzPngCollection.DisableAll;
begin
  EnableOrDisableAll(False);
end;

procedure TDzPngCollection.EnableAll;
begin
  EnableOrDisableAll(True);
end;

procedure TDzPngCollection.EnableOrDisableAll(const Enable: Boolean);
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do
    FItems[i].Enabled := Enable;
end;

function TDzPngCollection.IsValidIndex(const Index: integer): Boolean;
begin
  Result := Items.IsValidIndex(Index);
end;

function TDzPngCollection.AddPngImageFromFile(const FileName: string): integer;
var
  Png: TPngImage;
begin
  if not FileExists(FileName) then Exit(-1);
  Png := TPngImage.Create;
  try

    try
      Png.LoadFromFile(FileName);
      Result := AddPngImage(Png);
    except
      Result := -1;
    end;

  finally
    Png.Free;
  end;
end;

function TDzPngCollection.AddPngImage(Png: TPngImage; ImageName: string; Description: string; Enabled: Boolean; ATag: integer): integer;
var
  pci: TDzPngCollectionItem;
begin
  pci := Items.Add;
  try
    pci.PngImage.Assign(Png);
    Result := pci.Index;
    if ImageName <> '' then pci.Name := ImageName;
    pci.Description := Description;
    pci.Enabled := Enabled;
    pci.Tag := ATag;
  except
    Result := -1;
  end;
end;

function TDzPngCollection.GetPngImage(const Index: integer): TPngImage;
begin
  Result := Items[Index].PngImage;
end;

function TDzPngCollection.GetPngImageByName(const PngName: string; IgnoreCase: Boolean): TPngImage;
var
  x: integer;
begin
  x := PngIndex(PngName, IgnoreCase);
  if x < 0 then Exit(nil);
  Result := Items[x].PngImage;
end;

function TDzPngCollection.ReportStr: string;
const
  ENDL = sLineBreak;
var
  i: integer;
  s, Sep: string;
  pci: TDzPngCollectionItem;
  b: Boolean;

  function BoolToStrYN(const b: Boolean): string;
  begin
    if b then Result := 'Yes' else Result := 'No';
  end;
begin
  s :=
    'PngCollection: ' + Self.Name + ENDL +
    'Items: ' + IntToStr(Items.Count) + ENDL;

  Sep := '  ';
  for i := 0 to Items.Count - 1 do
  begin
    pci := Items[i];
    s := s + 'Item ' + IntToStr(i + 1) + ENDL;
    s := s + Sep + 'Index: ' + IntToStr(pci.Index) + ENDL;
    s := s + Sep + 'Name: ' + pci.Name + ENDL;
    s := s + Sep + 'Description: ' + pci.Description + ENDL;
    s := s + Sep + 'Enabled: ' + BoolToStrYN(pci.Enabled) + ENDL;
    s := s + Sep + 'Tag: ' + IntToStr(pci.Tag) + ENDL;
    b := pci.PngImage.Empty;
    s := s + Sep + 'Empy image: ' + BoolToStrYN(b) + ENDL;
    if b then Continue;
    s := s + Sep + 'Width: ' + IntToStr(pci.PngImage.Width) + ENDL;
    s := s + Sep + 'Height: ' + IntToStr(pci.PngImage.Height) + ENDL;
    {$IFDEF DCC}
    s := s + Sep + 'Bit depth: ' + IntToStr(pci.PngImage.Header.BitDepth) + ENDL;
    s := s + Sep + 'Compression level: ' + IntToStr(pci.PngImage.CompressionLevel) + ENDL;
    s := s + Sep + 'Compression method: ' + IntToStr(pci.PngImage.Header.CompressionMethod) + ENDL;
    //s := s + Sep + 'Transparent color: ' + ColorToRgbIntStr(pci.PngImage.TransparentColor) + ENDL;
    {$ENDIF}
  end;

  Result := s;
end;

function TDzPngCollection.PngIndex(const PngName: string; IgnoreCase: Boolean): integer;
var
  i: integer;
  s: string;
  b: Boolean;
begin
  Result := -1;
  if FItems.Count = 0 then Exit;
  s := PngName;
  if IgnoreCase then s := AnsiUpperCase(s);

  for i := 0 to FItems.Count - 1 do
  begin
    if IgnoreCase then b := s = AnsiUpperCase(FItems[i].Name)
    else b := s = FItems[i].Name;
    if b then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{$endregion TDzPngCollection}


{$region ' ------------------------ TDzPngCollectionItems ------------------------------ '}
constructor TDzPngCollectionItems.Create(AOwner: TPersistent);
begin
  inherited Create(TDzPngCollectionItem);
  FOwner := AOwner;
end;

function TDzPngCollectionItems.Add: TDzPngCollectionItem;
begin
  Result := TDzPngCollectionItem(inherited Add);
end;

procedure TDzPngCollectionItems.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  Update(nil);
end;

function TDzPngCollectionItems.IsValidIndex(const Index: integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Count);
end;

function TDzPngCollectionItems.GetItem(Index: Integer): TDzPngCollectionItem;
begin
  if IsValidIndex(Index) then Result := TDzPngCollectionItem(inherited Items[Index])
  else Result := nil;
end;

procedure TDzPngCollectionItems.SetItem(Index: Integer; const Value: TDzPngCollectionItem);
begin
  if IsValidIndex(Index) then inherited Items[Index] := Value;
end;

procedure TDzPngCollectionItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
end;

function TDzPngCollectionItems.Insert(Index: Integer): TDzPngCollectionItem;
begin
  Result := TDzPngCollectionItem(inherited Insert(Index));
end;

function TDzPngCollectionItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;
{$endregion TDzPngCollectionItems}


{$region ' ---------------------- TDzPngCollectionItem --------------------------- '}
constructor TDzPngCollectionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FPngImage := TPngImage.Create;
  FName := 'Png_' + Index.toString;
  FTag := 0;
  FEnabled := True;
end;

destructor TDzPngCollectionItem.Destroy;
begin
  FPngImage.Free;
  inherited Destroy;
end;

procedure TDzPngCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TDzPngCollectionItem then
  begin
    FPngImage.Assign(TDzPngCollectionItem(Source).PngImage);
    FName := TDzPngCollectionItem(Source).Name;
    FTag := TDzPngCollectionItem(Source).Tag;
    FEnabled := TDzPngCollectionItem(Source).Enabled;
  end
  else
    inherited Assign(Source);
end;

procedure TDzPngCollectionItem.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if (Dest is TDzPngCollectionItem) then TDzPngCollectionItem(Dest).PngImage := FPngImage;
end;

function TDzPngCollectionItem.GetDisplayName: string;
begin
  if Length(FName) = 0 then Result := inherited GetDisplayName
  else Result := FName;
end;

function TDzPngCollectionItem.GetHeight: integer;
begin
  if PngImage.Empty then Result := 0
  else Result := PngImage.Height;
end;

function TDzPngCollectionItem.GetWidth: integer;
begin
  if PngImage.Empty then Result := 0
  else Result := PngImage.Width;
end;

procedure TDzPngCollectionItem.SetPngImage(const Value: TPngImage);
begin
  if FPngImage = nil then FPngImage := TPngImage.Create;
  FPngImage.Assign(Value);
  FTag := 0;
  Changed(False);
end;


{$endregion TDzPngCollectionItem}




end.
