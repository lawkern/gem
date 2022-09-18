@ECHO off

SET COMPILER_FLAGS=-nologo -Z7 -Od -FC -diagnostics:column
SET LINKER_FLAGS=-incremental:no user32.lib

IF NOT EXIST ..\build mkdir ..\build
PUSHD ..\build

cl ..\code\platform_win32.c %COMPILER_FLAGS% -Fegem /link %LINKER_FLAGS%

POPD
