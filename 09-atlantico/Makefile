###############################################################################
# Rule to assemble and link all assembly files
###############################################################################
build:
	ca65 atlantico.asm -o atlantico.o
	ld65 -C nes.cfg atlantico.o -o atlantico.nes

###############################################################################
# Rule to remove all object (.o) and cartridge (.nes) files
###############################################################################
clean:
	rm *.o *.nes

###############################################################################
# Rule to run the final cartridge .nes file in the FCEUX emulator
###############################################################################
run:
	fceux atlantico.nes