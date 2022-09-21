@ECHO off

SET WIN32_ICON=201

SET COMPILER_FLAGS=-nologo -Z7 -Od -FC -diagnostics:column -DWIN32_ICON=%WIN32_ICON%
SET LINKER_FLAGS=-incremental:no user32.lib gdi32.lib comdlg32.lib comctl32.lib

IF NOT EXIST ..\build mkdir ..\build
PUSHD ..\build

REM NOTE(law): Compile any resources needed by the executable
rc -nologo -i ..\data -fo gem.res -d WIN32_ICON=%WIN32_ICON% ..\data\gem.rc

REM NOTE(law): Compile the actual executable
cl ..\code\platform_win32.c gem.res %COMPILER_FLAGS% -Fegem /link %LINKER_FLAGS%

POPD
