REM Requires ca65 bin dir to be in PATH

ca65 example.asm
ld65 -t nes -o example.nes example.o
PAUSE