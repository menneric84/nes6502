REM Requires ca65 bin dir to be in PATH

ca65 mini.asm
ld65 -t nes -o rom.nes mini.o
PAUSE