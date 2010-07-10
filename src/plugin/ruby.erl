% ex: set ts=2 noet:
% $Id$
% attempt to implement an environment for the evaluation of untrusted Ruby source code.
% relies on accompanying "exec" program for time-control
% Ruby's $SAFE Levels described: <URL: http://www.ruby-doc.org/docs/ProgrammingRuby/html/taint.html>

-module(ruby).
-export([
	test/0,
	loop/0,
	eval/1,
	eval_oops/1,
	escape/1
	]).
-include_lib("../irc.hrl").
-import(test).
-import(irc).

% ruby unit tests
test() ->
	Contains =
		fun(Expect, Result) ->
			(0 == length(Expect)) or
			(0 /= string:str(Result, Expect)) end,
	test:unit(ruby,
		[
			{ escape, test_escape() },
			{ eval, test_eval(), Contains },
			{ eval_oops, test_eval_oops(), Contains }
		]).

% receive loop
loop() ->
	receive
		{act, Pid, _Irc, Msg, Dst, Nick, ["ruby" | _]} ->
			act(Pid, Msg, Dst, Nick, Msg),
			loop();
		{act, _, _, _, _, _, _ } ->
			loop();
		{help, Pid, Dst, Nick} ->
			Pid ! {q,
				[ irc:resp(Dst, Nick, Nick ++ ": " ++ Msg)
					|| Msg <-
					[
						"[\"ruby\" | Code] -> evaluate ruby source code at $SAFE level 4",
						"Code = [ \"hello\", 1+1, [1,2,3].map{|x|x*x} ] % examples"
					]
				]
			},
			loop()
	end.

% evaluate the input as ruby source code
act(Pid, Msg, Dst, Nick, Msg) ->
	Rawtxt = Msg#ircmsg.rawtxt,
	Source = string:substr(Rawtxt, length("ruby ")),
	io:format("ruby Source=~p~n", [Source]),
	Output = ruby:eval(Source), % we get at most one line of output
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Output)}.

eval(Ruby) ->
  Run = "./exec /usr/bin/env ruby \"-e puts Thread.start{ \\$SAFE=4; " ++ escape(Ruby) ++ " }.join.value.inspect\"",
  %io:format("Ruby=<~s> Run=~p~n", [Ruby, Run]), % print all commands, so we can see 
  Out = os:cmd(Run),
	Lines = string:tokens(Out, "\r\n"),
	if
		[] == Lines -> "";
		true -> ircutil:dotdotdot(hd(Lines), 200)
	end.

test_eval() ->
	[
		% see what our output is for various weird, sometimes-legitimate code snippets
		% NOTE: we can't rely on ruby's exact output for error messages, it is inconsistent between 1.8 and 1.9
		% and both are widely used
  	{ [ "" ],															"4" },
  	%{ [ "." ],														"-e:1: syntax error, unexpected '.', expecting '}'" },
  	%{ [ ":" ],														"-e:1: syntax error, unexpected ':', expecting '}'" },
  	%{ [ "<" ],														"-e:1: syntax error, unexpected '<', expecting '}'" },
  	{ [ "\b" ],														"Invalid char" },
  	%{ [ "$" ],														"-e:1: syntax error, unexpected $undefined, expecting '}'" },
  	%{ [ "}" ],														"-e:1: syntax error, unexpected '}', expecting $end" },
  	%{ [ "{" ],														"-e:1: syntax error, unexpected $end, expecting '}'" },
  	{ [ "{}" ],														"{}" },
  	{ [ "$$" ],														"Insecure operation at level 4" },
		{ [ string:chars(32, 50) ],						"4" },
		{ [ string:chars($0, 50) ],						"0" },
		{ [ string:chars($a, 50) ],						"undefined local variable or method `" ++ string:chars($a, 50) ++ "' for main:Object (NameError)" },
		% correct output for legitimate code
  	{ [ "[1,2,3].collect{|i|i*i}" ], 			"[1, 4, 9]" },
		% see what's allowed
  	{ [ "\$SAFE=0;" ],										"Insecure: can't change global variable value (SecurityError)" },
  	{ [ "exit" ],													"Insecure operation" },
  	{ [ "abort" ],												"Insecure operation" },
  	{ [ "def b();end;" ],									"Insecure: can't define method (SecurityError)" },
  	{ [ "Thread.new{}.nil?" ],						"false" },
		% prevent external command execution
		% via the shell..
  	{ [ "p `ls`" ],												"Insecure operation" },
		% via ruby...
  	{ [ "p 'ls'" ],												"Insecure operation" },
  	{ [ "puts `ls`" ],										"Insecure operation" },
		% prevent source exhaustion - time
  	{ [ "sleep" ], 												"timeout" },
  	{ [ "while(1)do;end" ], 							"timeout" },
  	{ [ "x=lambda{|x|x.call(x)};x.call(x)" ],"" },
		% this fails sometimes, not sure why
  	%{ [ "(0..2147483648).collect{|x|x*2}" ],"timeout" },
		% prevent resource exhaustion - memory
  	{ [ "(0..2147483648)" ],							"0..2147483648" },
  	{ [ "fork" ],													"Insecure operation" },
		% this fails sometimes, not sure why
  	%{ [ "while(1)do;Thread.new{}end" ],		"timeout" },
  	{ [ "while(1)do;fork;end" ],					"Insecure operation" }
	].

% what happens if the 'exec' program doesn't exist?
% simulate this by trying to run the non-existent "exec2" program
% we should behave reasonably
eval_oops(Ruby) ->
  Run = "./exec2 /usr/bin/env ruby \"-e puts Thread.start{ \\$SAFE=4; " ++ escape(Ruby) ++ " }.join.value.inspect\"",
  io:format("Ruby=<~s> Run=~p~n", [Ruby, Run]), % print all commands, so we can see 
  Out = os:cmd(Run),
	Lines = string:tokens(Out, "\r\n"),
	hd(Lines).

test_eval_oops() ->
	[
  	{ [ "" ],															"./exec2: " }
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

