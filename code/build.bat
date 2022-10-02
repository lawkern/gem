@ECHO off

SET DEVELOPMENT_BUILD=1

SET WARNING_FLAGS=-WX -W4 -wd4201 -wd4204
IF %DEVELOPMENT_BUILD%==1 (
   SET WARNING_FLAGS=%WARNING_FLAGS% -wd4100 -wd4101 -wd4189
)

SET RESOURCES=-DWIN32_ICON=201 -DWIN32_TOOLBAR_BUTTONS_BITMAP=202

SET COMPILER_FLAGS=-nologo -Z7 -Oi -Od -FC -MT -diagnostics:column %WARNING_FLAGS% %RESOURCES%
SET LINKER_FLAGS=-opt:ref -incremental:no user32.lib gdi32.lib winmm.lib comdlg32.lib comctl32.lib

IF NOT EXIST ..\build mkdir ..\build
PUSHD ..\build

REM NOTE(law): Compile any resources needed by the executable
rc -nologo -i ..\data -fo gem.res %RESOURCES% ..\data\gem.rc

REM NOTE(law): Compile the actual executable
cl ..\code\platform_win32.c gem.res %COMPILER_FLAGS% -Fegem /link %LINKER_FLAGS%

POPD
