RESULT	=	step3
SOURCES =	door.mli door.ml \
		cell.mli cell.ml \
		maze.mli maze.ml \
		displayer.mli displayer.ml \
		main.ml

LIBS	=	bigarray sdl
INCDIRS	=	+sdl

all	:	nc

byte	:	bc

fclean	: 	clean
		rm -f $(RESULT)

re	:	fclean all

include OCamlMakefile
