@echo off
cd Driver
sjasmplus --lst=Driver.lst Driver.asm
IF ERRORLEVEL 1 GOTO error
cd ..
Nextor\mknexrom Nextor\Nextor-2.1-alpha2.base.dat Driver\SDMAPPER.ROM /d:Driver\driver.bin /m:Nextor\Mapper.ASCII16.bin
IF ERRORLEVEL 1 GOTO error
goto ok

:error
echo Ocorreu algum erro!
:ok
echo.
pause