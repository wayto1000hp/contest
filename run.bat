@echo on
setlocal
if not exist program.exe echo program.exe not found & pause & exit /b 1
program.exe
echo -------- output.txt --------
type output.txt
pause
endlocal
