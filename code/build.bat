@ECHO off

SET DEVELOPMENT_BUILD=1

SET WARNING_FLAGS=-WX -W4 -wd4201 -wd4204
IF %DEVELOPMENT_BUILD%==1 (
   SET WARNING_FLAGS=%WARNING_FLAGS% -wd4100
)

SET WIN32_ICON=201
SET COMPILER_FLAGS=-nologo -Z7 -Oi -Od -FC -MT -diagnostics:column -DWIN32_ICON=%WIN32_ICON% %WARNING_FLAGS%
SET LINKER_FLAGS=-opt:ref -incremental:no user32.lib gdi32.lib comdlg32.lib comctl32.lib

IF NOT EXIST ..\build mkdir ..\build
PUSHD ..\build

REM NOTE(law): Compile any resources needed by the executable
rc -nologo -i ..\data -fo gem.res -d WIN32_ICON=%WIN32_ICON% ..\data\gem.rc

REM NOTE(law): Compile the actual executable
cl ..\code\platform_win32.c gem.res %COMPILER_FLAGS% -Fegem /link %LINKER_FLAGS%

POPD
