unit DzHTMLText2_Register;

{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  //Forms, Classes, Graphics, Controls, SysUtils, StdCtrls,
  Classes,
  {$IFDEF FPC}LCLIntf, LCLType, LMessages, lresources, {$ENDIF}
  //TypInfo,
  DzHTMLText2, DzPngCollection
  ;

procedure Register;

implementation

procedure Register;

begin
  RegisterComponents('Digao', [TDzHTMLText2, TDzPngCollection]);
end;


end.
