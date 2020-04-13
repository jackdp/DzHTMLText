unit DzHTMLText2_Helpers;

{$I dz2.inc}

interface

uses
  {$IFDEF DELPHI2009}Generics.Collections,{$ENDIF}
  Types;


{$IFDEF DELPHIXE_OR_BELOW}

type

  TPointHelper = record helper for TPoint
  public
    constructor Create(P: TPoint); overload;
    constructor Create(const X, Y: Integer); overload;
    procedure Offset(const DX, DY: Integer); overload;
    procedure Offset(const Point: TPoint); overload;
  end;

  TRectHelper = record helper for TRect
  private
    function GetWidth: Integer;
    procedure SetWidth(const Value: Integer);
    function GetHeight: Integer;
    procedure SetHeight(const Value: Integer);
  public
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;

    function Contains(const Pt: TPoint): Boolean; overload;
    function Contains(const R: TRect): Boolean; overload;
    procedure Offset(const DX, DY: Integer); overload;
    procedure Offset(const Point: TPoint); overload;
  end;


  TSizeHelper = record helper for TSize
  private
    function GetWidth: integer;
    procedure SetWidth(const Value: integer);
    function GetHeight: integer;
    procedure SetHeight(const Value: integer);
  public
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
  end;

{$ENDIF}


{$IFDEF DELPHI2009}
type
  TList<T> = class(Generics.Collections.TList<T>)
  public
    function First: T; inline;
    function Last: T; inline;
  end;

  TObjectList<T: class> = class(Generics.Collections.TObjectList<T>)
  public
    function Last: T; inline;
  end;
{$ENDIF}



implementation


{$IFDEF DELPHI2009}
function TList<T>.First: T;
begin
  Result := Items[0];
end;

function TList<T>.Last: T;
begin
  Result := Items[Count - 1];
end;


function TObjectList<T>.Last: T;
begin
  Result := Items[Count - 1];
end;
{$ENDIF}


{$IFDEF DELPHIXE_OR_BELOW}

// ---------------------- TPointHelper -------------------------

constructor TPointHelper.Create(P: TPoint);
begin
  Self.X := P.X;
  Self.Y := P.Y;
end;

constructor TPointHelper.Create(const X, Y: Integer);
begin
  Self.X := X;
  Self.Y := Y;
end;

procedure TPointHelper.Offset(const DX, DY: Integer);
begin
  Inc(Self.X, DX);
  Inc(Self.Y, DY);
end;

procedure TPointHelper.Offset(const Point: TPoint);
begin
  Self.Offset(Point.X, Point.Y);
end;


// ----------------------- TRectHelper ---------------------------

function TRectHelper.GetHeight: Integer;
begin
  Result := Self.Bottom - Self.Top;
end;

procedure TRectHelper.SetHeight(const Value: Integer);
begin
  Self.Bottom := Self.Top + Value;
end;

function TRectHelper.GetWidth: Integer;
begin
  Result := Self.Right - Self.Left;
end;

procedure TRectHelper.SetWidth(const Value: Integer);
begin
  Self.Right := Self.Left + Value;
end;

function TRectHelper.Contains(const PT: TPoint): Boolean;
begin
  Result := PtInRect(Self, PT);
end;

function TRectHelper.Contains(const R: TRect): Boolean;
begin
  Result := (Self.Left <= R.Left) and (Self.Right >= R.Right) and (Self.Top <= R.Top) and (Self.Bottom >= R.Bottom);
end;

procedure TRectHelper.Offset(const DX, DY: Integer);
begin
  TopLeft.Offset(DX, DY);
  BottomRight.Offset(DX, DY);
end;

procedure TRectHelper.Offset(const Point: TPoint);
begin
  TopLeft.Offset(Point);
  BottomRight.Offset(Point);
end;



// ----------------------- TSizeHelper ----------------------------

function TSizeHelper.GetWidth: integer;
begin
  Result := Self.cx;
end;

procedure TSizeHelper.SetWidth(const Value: integer);
begin
  Self.cx := Value;
end;

function TSizeHelper.GetHeight: integer;
begin
  Result := Self.cy;
end;

procedure TSizeHelper.SetHeight(const Value: integer);
begin
  Self.cy := Value;
end;

{$ENDIF} // DELPHIXE_OR_BELOW


end.