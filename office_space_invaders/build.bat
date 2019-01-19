REM Requires ca65 bin dir to be in PATH

ca65 office_space_invaders.asm
ld65 -C nesmem.cfg -o office_space_invaders.nes office_space_invaders.o
PAUSE