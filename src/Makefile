# ex: set ts=8 noet:

CFLAGS = -W -Wall -Werror -pedantic
ERLC = erlc

all: exec htmlparser
	$(ERLC) *.erl

exec: exec.o

htmlparser:
	$(ERLC) -o htmlparser htmlparser/*.erl

clean:
	$(RM) *.beam exec

