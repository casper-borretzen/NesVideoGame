##############################################################################
# Rule to assemble and link all assembly files
##############################################################################
all:
	ca65 clearmem.asm -o clearmem.o
	ld65 clearmem.o   -o clearmem.nes -C nes.cfg

##############################################################################
# Rule to remove all object (.o) and cartridge (.nes) files
##############################################################################
clean:
	rm *.o *.nes

##############################################################################
# Rule to run the final cartridge .nes file in the FCEUX emulator
##############################################################################
run:
	fceux clearmem.nes
