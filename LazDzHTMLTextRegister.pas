unit LazDzHTMLTextRegister;

{$mode objfpc}{$H+}

interface

uses
    Forms, LCLIntf, LCLType, LMessages, Classes, Graphics, Controls, SysUtils, StdCtrls,
    TypInfo, lresources,
    DzHTMLText, DzPngCollection
    ;

procedure Register;

implementation

procedure Register;

begin
  RegisterComponents('Digao', [TDzHTMLText, TDzPngCollection]);
end;


end.
