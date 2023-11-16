@echo off
ECHO generated on host: WPIA-DIDE276
ECHO generated on date: 2023-06-14
ECHO didehpc version: 0.3.18
ECHO conan version: 0.1.1
ECHO running on: %COMPUTERNAME%
call setr64_4_3_0.bat
ECHO mapping Q: -^> \\fi--san03.dide.ic.ac.uk\homes\eknock
net use Q: \\fi--san03.dide.ic.ac.uk\homes\eknock /y
ECHO mapping T: -^> \\fi--didef3.dide.ic.ac.uk\tmp
net use T: \\fi--didef3.dide.ic.ac.uk\tmp /y
ECHO mapping Z: -^> \\wpia-hn.hpc.dide.ic.ac.uk\ncov-rtm
net use Z: \\wpia-hn.hpc.dide.ic.ac.uk\ncov-rtm /y
set CONAN_PATH_BOOTSTRAP=T:\conan\bootstrap\4.3
set CONAN_PATH_CACHE=Z:\Ed\ncov-outputs\src\comparison_sir_filter_test\contexts\conan\cache
set CONAN_ID=81bcbd7adcd25236af4a5e9d3343b02e
set CONAN_LOGFILE=Z:\Ed\ncov-outputs\src\comparison_sir_filter_test\contexts\conan\log\%CONAN_ID%
ECHO logfile: %CONAN_LOGFILE%
Z:
cd \Ed\ncov-outputs\src\comparison_sir_filter_test
ECHO working directory: %CD%
ECHO on
Rscript "Z:\Ed\ncov-outputs\src\comparison_sir_filter_test\contexts\conan\bin\%CONAN_ID%" "Z:\Ed\ncov-outputs\src\comparison_sir_filter_test\contexts\lib\windows\4.3" > "%CONAN_LOGFILE%" 2>&1
@ECHO off
%SystemDrive%
set ErrorCode=%ERRORLEVEL%
ECHO Removing mapping Q:
net use Q: /delete /y
ECHO Removing mapping T:
net use T: /delete /y
ECHO Removing mapping Z:
net use Z: /delete /y
set ERRORLEVEL=%ErrorCode%
if %ERRORLEVEL% neq 0 (
  ECHO Error running conan
  EXIT /b %ERRORLEVEL%
)
@ECHO Quitting
