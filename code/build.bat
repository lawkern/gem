@ECHO off

SET COMPILER_FLAGS=-nologo -Z7 -Od -FC -diagnostics:column
SET LINKER_FLAGS=-incremental:no

IF NOT EXIST ..\build mkdir ..\build
PUSHD ..\build

cl ..\code\gem.c %COMPILER_FLAGS% -Fegem /link %LINKER_FLAGS%

POPD
