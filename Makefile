##
## EPITECH PROJECT, 2021
## makefile
## File description:
## makefile
##

STACK_PATH := $(shell stack path --local-install-root)

NAME = koak

MAIN    := app/Main.hs

SRC     := src/Data.hs

TESTSRC := test/Spec.hs

OBJ := $(MAIN:.hs=.o) $(MAIN:.hs=.hi)
OBJ += $(SRC:.hs=.o) $(SRC:.hs=.hi)
OBJ += $(TESTSRC:.hs=.o) $(TESTSRC:.hs=.hi)


all: $(SRC) $(MAIN)
	stack build


	cp $(STACK_PATH)/bin/koak-exe ./$(NAME)

tests_run:
	stack test

clean:
	stack clean

fclean:	clean
	$(RM) $(NAME)

re:	fclean all

.PHONY: all tests_run clean fclean re