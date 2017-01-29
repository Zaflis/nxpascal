@echo off
echo *** Clean batch for delphi demos ***
IF NOT EXIST demos\delphi GOTO ERR
echo Folder found, starting cleaning
cd demos\delphi

del *.exe /s
del *.map /s
del *.dsk /s
del *.dcu /s
del *.drc /s
del thumbs.db /s
del *.~* /s
cd ..

GOTO END
:ERR
echo Folder delphi_demos not found!
:END
echo Batch quit
pause