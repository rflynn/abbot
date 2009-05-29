% ex: set ts=2 noet:
% $Id$
% attempt to implement an environment for the evaluation of untrusted Ruby source code.
% relies on accompanying "exec" program for time-control
% Ruby's $SAFE Levels described: <URL: http://www.ruby-doc.org/docs/ProgrammingRuby/html/taint.html>

-module(ruby).
-export([test/0, eval/1, eval_oops/1, escape/1]).
-import(test).

test() ->
	test:unit(ruby,
		[
			{ escape, test_escape() },
			{ eval, test_eval() },
			{ eval_oops, test_eval_oops() }
		]).

eval(Ruby) ->
  Run = "./exec /usr/bin/ruby \"-e puts Thread.start{ \\$SAFE=4; " ++ escape(Ruby) ++ " }.join.value.inspect\"",
  io:format("Ruby=<~s> Run=~p~n", [Ruby, Run]), % print all commands, so we can see 
  Out = os:cmd(Run),
	Lines = string:tokens(Out, "\r\n"),
	hd(Lines).

test_eval() ->
	[
		% see what our output is for various weird, sometimes-legitimate code snippets
  	{ [ "" ],															"4" },
  	{ [ "." ],														"-e:1: syntax error, unexpected '.', expecting '}'" },
  	{ [ ":" ],														"-e:1: syntax error, unexpected ':', expecting '}'" },
  	{ [ "<" ],														"-e:1: syntax error, unexpected '<', expecting '}'" },
  	{ [ "\b" ],														"-e:1: Invalid char `\\010' in expression" },
  	{ [ "$" ],														"-e:1: syntax error, unexpected $undefined, expecting '}'" },
  	{ [ "}" ],														"-e:1: syntax error, unexpected '}', expecting $end" },
  	{ [ "{" ],														"-e:1: syntax error, unexpected $end, expecting '}'" },
  	{ [ "{}" ],														"{}" },
  	{ [ "$$" ],														"-e:1: Insecure operation at level 4 (SecurityError)" },
		% correct output for legitimate code
  	{ [ "[1,2,3].collect{|i|i*i}" ], 			"[1, 4, 9]" },
		% see what's allowed
  	{ [ "\$SAFE=0;" ],										"-e:1: Insecure: can't change global variable value (SecurityError)" },
  	{ [ "exit" ],													"-e:1:in `exit': Insecure operation `exit' at level 4 (SecurityError)" },
  	{ [ "abort" ],												"-e:1:in `abort': Insecure operation `abort' at level 4 (SecurityError)" },
  	{ [ "def b();end;" ],									"-e:1: Insecure: can't define method (SecurityError)" },
  	{ [ "Thread.new{}.nil?" ],						"false" },
		% prevent external command execution
		% via the shell..
  	{ [ "p `ls`" ],												"-e:1:in ``': Insecure operation - ` (SecurityError)" },
		% via ruby...
  	{ [ "p 'ls'" ],												"-e:1:in `write': Insecure operation `write' at level 4 (SecurityError)" },
  	{ [ "puts `ls`" ],										"-e:1:in ``': Insecure operation - ` (SecurityError)" },
		% prevent source exhaustion - time
  	{ [ "sleep" ], 												"timeout" },
  	{ [ "while(1)do;end" ], 							"timeout" },
  	{ [ "x=lambda{|x|x.call};x.call(x)" ],"-e:1:in `write': Insecure operation `write' at level 4 (SecurityError)" },
  	{ [ "(0..2147483648).collect{|x|x*2}" ],"timeout" },
		% prevent resource exhaustion - memory
  	{ [ "(0..2147483648)" ],							"0..2147483648" },
  	{ [ "fork" ],													"-e:1:in `fork': Insecure operation `fork' at level 4 (SecurityError)" },
  	{ [ "while(1)do;Thread.new{}end" ],		"timeout" },
  	{ [ "while(1)do;fork;end" ],					"-e:1:in `fork': Insecure operation `fork' at level 4 (SecurityError)" }
	].

% what happens if the 'exec' program doesn't exist?
% simulate this by trying to run the non-existent "exec2" program
% we should behave reasonably
eval_oops(Ruby) ->
  Run = "./exec2 /usr/bin/ruby \"-e puts Thread.start{ \\$SAFE=4; " ++ escape(Ruby) ++ " }.join.value.inspect\"",
  io:format("Ruby=<~s> Run=~p~n", [Ruby, Run]), % print all commands, so we can see 
  Out = os:cmd(Run),
	Lines = string:tokens(Out, "\r\n"),
	hd(Lines).

test_eval_oops() ->
	[
  	{ [ "" ],															"/bin/sh: line 1: ./exec2: No such file or directory" }
	].

% prepare ruby code for being run in a shell
% escape: ", $
escape(Str) ->
  %{ok, Match} = re:compile("[\"$`]"),
  %re:replace(Str, Match,"\\&", [{return, list}]).
	% bloody hell, the re module doesn't seem to be working
  esc_(Str, []).

esc_([], Esc) ->
  Esc;
esc_("`" ++ Unesc, Esc) ->
  esc_(Unesc, Esc ++ "\\\`");
esc_("\"" ++ Unesc, Esc) ->
  esc_(Unesc, Esc ++ "\\\"");
esc_("$" ++ Unesc, Esc) ->
  esc_(Unesc, Esc ++ "\\$");
esc_([C|Unesc], Esc) ->
  esc_(Unesc, Esc ++ [C]).

test_escape() ->
	[
  	{ [ "" ], "" },
  	{ [ " " ], " " },
  	{ [ "\"" ], "\\\"" },
  	{ [ "`ls`" ], "\\\`ls\\\`" },
  	{ [ "$SAFE=4" ], "\\\$SAFE=4" }
	].



