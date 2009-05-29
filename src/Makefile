# ex: set ts=8 noet:

CFLAGS = -W -Wall -Werror -pedantic

all: exec
	erlc *.erl

exec: exec.o

clean:
	$(RM) *.beam exec

