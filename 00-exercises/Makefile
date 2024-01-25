##############################################################################
# Rule to assemble and link all exercise files
##############################################################################
all:
	ca65 exercise1.asm -o exercise1.o
	ld65 exercise1.o   -o exercise1.nes -C nes.cfg
	
	ca65 exercise2.asm -o exercise2.o
	ld65 exercise2.o   -o exercise2.nes -C nes.cfg
	
	ca65 exercise3.asm -o exercise3.o
	ld65 exercise3.o   -o exercise3.nes -C nes.cfg
	
	ca65 exercise4.asm -o exercise4.o
	ld65 exercise4.o   -o exercise4.nes -C nes.cfg
	
	ca65 exercise5.asm -o exercise5.o
	ld65 exercise5.o   -o exercise5.nes -C nes.cfg
	
	ca65 exercise6.asm -o exercise6.o
	ld65 exercise6.o   -o exercise6.nes -C nes.cfg
	
	ca65 exercise7.asm -o exercise7.o
	ld65 exercise7.o   -o exercise7.nes -C nes.cfg
	
	ca65 exercise8.asm -o exercise8.o
	ld65 exercise8.o   -o exercise8.nes -C nes.cfg
	
	ca65 exercise9.asm -o exercise9.o
	ld65 exercise9.o   -o exercise9.nes -C nes.cfg

##############################################################################
# Rule to assemble and link all exercise files
##############################################################################
clean:
	rm *.o *.nes
