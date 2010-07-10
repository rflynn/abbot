% ex: set ts=2 noet:
% $Id$
% attempt to implement an environment for the evaluation of untrusted Haskell source code.
% relies on accompanying "exec" program for time-control

-module(haskell).
-export([
	test/0,
	loop/0,
	eval/1,
	eval_oops/1
	]).
-include_lib("../irc.hrl").
-import(test).
-import(util).
-import(irc).

% unit tests
test() ->
	Contains =
		fun(Expect, Result) ->
			(0 == length(Expect)) or
			(0 /= string:str(Result, Expect)) end,
	test:unit(haskell,
		[
			%{ eval, test_eval(), Contains },
			{ eval_oops, test_eval_oops(), Contains }
		]).

% receive loop
loop() ->
	receive
		{act, Pid, _Irc, Msg, Dst, Nick, [">" | _]} ->
			act(Pid, Msg, Dst, Nick, Msg),
			loop();
		{act, _, _, _, _, _, _ } ->
			loop();
		{help, Pid, Dst, Nick} ->
			Pid ! {q,
				[ irc:resp(Dst, Nick, Nick ++ ": " ++ Msg)
					|| Msg <-
					[
						"[\">\" | Code] -> evaluate Haskell source",
						"Code = [ 1+1; \"hu\" ++ cycle \"r\" ] -- examples"
					]
				]
			},
			loop()
	end.

% evaluate the input as source code
act(Pid, Msg, Dst, Nick, Msg) ->
	Rawtxt = Msg#ircmsg.rawtxt,
	Source = string:substr(Rawtxt, length("> ")),
	io:format("haskell Source=~p~n", [Source]),
	Output = haskell:eval(Source), % we get at most one line of output
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Output)}.

% $ echo "1+1" | ghci | grep ^Prelude | head -n 1
% Prelude> 2
make_cmd(UnsafeCode) ->
	SafeCode = shell_escape(UnsafeCode),
	% FIXME: does not report errors, which occur on the second line...
  "cat <<EOS | ./exec /usr/bin/env ghci | grep ^Prelude | head -n 1 | egrep --only \"> .*?\" | cut -c 3-\n" ++ "import List\n" ++ "import Data.List\n" ++ SafeCode ++ "\nEOS\n".

%cat <<EOS | ghci | grep ^Prelude | head -n 1 | egrep --only "> (.*)" | cut -c 3-
%import Data.List
%intercalate "x" "a" "b"
%EOS

% "Prelude List> Prelude List Data.List> 0" -> "0"
strip_prelude(Str) ->
	case string:substr(Str, 1, 8) of
		"Prelude " ->
			Strip = string:substr(Str, string:str(Str, "> ")+2),
			strip_prelude(Strip);
		_ -> Str
	end.

eval(HaskellSrc) ->
	Cmd = make_cmd(HaskellSrc),
  io:format("HaskellSrc=<~s> Cmd=~s~n", [HaskellSrc, Cmd]), % print all commands, so we can see 
  Out = os:cmd(Cmd),
	Lines = string:tokens(Out, "\r\n"),
	if
		[] == Lines -> "";
		true -> ircutil:dotdotdot(strip_prelude(hd(Lines)), 100)
	end.

% prepare code for being run in a shell
% escape: ", $
shell_escape(Str) ->
  esc_(Str, []).

esc_([], Esc) ->
  Esc;
esc_("`" ++ Unesc, Esc) ->
  esc_(Unesc, Esc ++ "\\\`");
esc_("$(" ++ Unesc, Esc) ->
  esc_(Unesc, Esc ++ "$ (");
esc_([C|Unesc], Esc) ->
  esc_(Unesc, Esc ++ [C]).

test_eval() ->
	[
		% see what our output is for various weird, sometimes-legitimate code snippets
  	{ [ "" ],															"" }
	].

% what happens if the 'exec' program doesn't exist?
% simulate this by trying to run the non-existent "exec2" program
% we should behave reasonably
eval_oops(Src) ->
  Run = "./exec2 /usr/bin/env ghci",
  io:format("Haskell=<~s> Run=~p~n", [Src, Run]), % print all commands, so we can see 
  Out = os:cmd(Run),
	Lines = string:tokens(Out, "\r\n"),
	hd(Lines).

test_eval_oops() ->
	[
  	{ [ "" ],															"./exec2: " }
	].
