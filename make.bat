rem echo off
rem MAKE.BAT for APEL.
rem ********************************************************************
rem * Edit following lines to set PREFIX, EMACS, LISPDIR and VLISPDIR  *
rem * according to your environment                                    *
rem ********************************************************************
rem * for Meadow                                                       *
rem *   If you use Meadow on Windows NT, use meadowNT.exe insted of    *
rem *   meadow95.exe                                                   *
set MEADOWVER=1.10
set PREFIX=c:\usr\meadow
set EMACS=%PREFIX%\%MEADOWVER%\bin\meadow95.exe
set LISPDIR=%PREFIX%\site-lisp
set VLISPDIR=%PREFIX%\%MEADOWVER%\site-lisp
rem 
rem ********************************************************************
rem * for Mule for Windows                                             *
rem set PREFIX=c:\usr\mule
rem set EMACS=%PREFIX%\bin\mule.exe
rem set LISPDIR=%PREFIX%\site-lisp
rem set VLISPDIR=%LISPDIR%
rem 
rem ********************************************************************
rem * for Demacs                                                       *
rem *  Please add examples for Demacs                                  *
rem 
rem ********************************************************************

set arg1=%1
if "%arg1%"=="install" goto install
if "%arg1%"=="what-where" goto listing
if "%arg1%"=="clean" goto clean

:install
%EMACS% -q -batch -no-site-file -l APEL-MK -f compile-apel NONE %LISPDIR% %VLISPDIR%
%EMACS% -q -batch -no-site-file -l APEL-MK -f install-apel NONE %LISPDIR% %VLISPDIR%
goto end

:listing
%EMACS% -batch -q -no-site-file -l APEL-MK -f what-where-apel
goto end

:clean
del *.elc
del *~
del .#~

:end

