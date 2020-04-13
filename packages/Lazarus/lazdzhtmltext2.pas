{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazDzHTMLText2;

{$warn 5023 off : no warning about unused units}
interface

uses
  DzHTMLText2_Register, DzHTMLText2_Helpers, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DzHTMLText2_Register', @DzHTMLText2_Register.Register);
end;

initialization
  RegisterPackage('LazDzHTMLText2', @Register);
end.
