##############################################################################
# Rule to assemble and link all assembly files
##############################################################################
all:
	ca65 helloppu.asm -o helloppu.o
	ld65 helloppu.o   -o helloppu.nes -C nes.cfg

##############################################################################
# Rule to remove all object (.o) and cartridge (.nes) files
##############################################################################
clean:
	rm *.o *.nes

##############################################################################
# Rule to run the final cartridge .nes file in the FCEUX emulator
##############################################################################
run:
	fceux helloppu.nes
