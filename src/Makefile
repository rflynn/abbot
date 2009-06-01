# ex: set ts=8 noet:

CFLAGS = -W -Wall -Werror -pedantic
ERL = erl

all: exec
	$(ERL) -make

exec: exec.o

clean:
	$(RM) *.o exec *.beam htmlparser/*.beam plugin/*.beam

