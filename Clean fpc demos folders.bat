@echo off
echo *** Clean batch for fpc demos ***
IF NOT EXIST demos\fpc GOTO ERR
echo Folder found, starting cleaning
cd demos\fpc

del *.dbg /s
del *.exe /s
del *.ico /s
del *.o /s
del *.ppu /s
del *.compiled /s
del *.lrs /s
del *.lps /s
del *.res /s
del *.bak /s
del thumbs.db /s

cd custom_window
rmdir backup /s /q
rmdir lib /s /q
cd ..

cd effects1
rmdir backup /s /q
rmdir lib /s /q
cd ..

cd fps
rmdir backup /s /q
rmdir lib /s /q
cd ..

cd framebuffer
rmdir backup /s /q
rmdir lib /s /q
cd ..

cd gametemplate
rmdir backup /s /q
rmdir lib /s /q
cd ..

cd model
rmdir backup /s /q
rmdir lib /s /q
cd ..

cd networkTest
rmdir backup /s /q
rmdir lib /s /q
cd ..

cd pathfind
rmdir backup /s /q
rmdir lib /s /q
cd ..

cd picking
rmdir backup /s /q
rmdir lib /s /q
cd ..

cd shader
rmdir backup /s /q
rmdir lib /s /q
cd ..

cd texture
rmdir backup /s /q
rmdir lib /s /q
cd ..

cd walker
rmdir backup /s /q
rmdir lib /s /q
cd ..

cd walker_shader
rmdir backup /s /q
rmdir lib /s /q
cd ..

GOTO END
:ERR
echo Folder fpc_demos not found!
:END
echo Batch quit
pause