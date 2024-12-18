@echo off
set FORMAT="c1541.exe" -format "disk,0" d81 "DEMO.D81"
set WRITE="c1541.exe" -attach "DEMO.D81" 8 -write

set M65="C:\Program Files\xemu\xmega65.exe" -besure -8 "DEMO.D81" -autoload

set DEPLOY="C:\Users\schol\Documents\MEGA65\m65tools-develo-180-a2da95-windows\m65.exe" -l COM7 -F -r -virtuald81 "DEMO.D81"
set SEND="C:\Users\schol\Documents\MEGA65\m65tools-develo-180-a2da95-windows\mega65_ftp.exe" -l COM7 -c "put DEMO.D81" -c "mount DEMO.D81" -c "quit" 
set TYPE="C:\Users\schol\Documents\MEGA65\m65tools-develo-180-a2da95-windows\m65.exe" -l COM7
set RESET="C:\Users\schol\Documents\MEGA65\m65tools-develo-180-a2da95-windows\m65.exe" -l COM7 -F1

%FORMAT% && %WRITE% "demo.prg" && %WRITE% "assets\bin\pal\aurora.bin" && %WRITE% "assets\bin\images\tiles.bin" && %WRITE% "assets/bin/maps/ui.bin" && %WRITE% "assets/bin/maps/map.bin"

echo "Start M65 Emulator"
%M65%

%RESET%
%SEND%
%TYPE% -T 'list'
%TYPE% -T 'list'
%TYPE% -T 'load\"$\",8'
%TYPE% -T 'list'
%TYPE% -T 'list'
%TYPE% -T 'list'
%TYPE% -T 'load\"demo.prg\"'
%TYPE% -T 'run'