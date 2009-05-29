# ex: set ts=8 noet:

CFLAGS = -W -Wall -Werror -pedantic

all:
	$(CC) -o exec exec.c
	erlc *.erl

clean:
	$(RM) *.beam exec

