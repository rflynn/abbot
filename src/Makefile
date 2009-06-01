# ex: set ts=8 noet:

CFLAGS = -W -Wall -Werror -pedantic
ERLC = erlc

all: exec htmlparser
	$(ERLC) -o plugin plugin/*.erl
	$(ERLC) *.erl
	$(ERLC) -o htmlparser htmlparser/*.erl

exec: exec.o

clean:
	$(RM) *.o exec *.beam plugin/*.beam

