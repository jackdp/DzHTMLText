@echo off

set Template=DzHTMLText2_TEMPLATE.dpk
set Desc="Digao - HTML Label Component"
set BaseOutDir=..\
set ShortDpkName=DzHTMLText2.dpk

for %%x in (2009,2010,XE,XE2,XE3,XE4,XE5,XE6,XE7,XE8,Seattle,Berlin,Tokyo,Rio) do (
  DpkGen -t %Template% -d %Desc% -o %BaseOutDir%%%x\%ShortDpkName% -s _%%x
)

