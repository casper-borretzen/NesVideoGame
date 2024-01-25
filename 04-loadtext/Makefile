##############################################################################
# Rule to assemble and link all assembly files
##############################################################################
all:
	ca65 loadtext.asm -o loadtext.o
	ld65 loadtext.o   -o loadtext.nes -C nes.cfg

##############################################################################
# Rule to remove all object (.o) and cartridge (.nes) files
##############################################################################
clean:
	rm *.o *.nes

##############################################################################
# Rule to run the final cartridge .nes file in the FCEUX emulator
##############################################################################
run:
	fceux loadtext.nes
