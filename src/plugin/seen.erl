% ex: set ts=2 noet:

% remember the last time i've seen people based on nick

-module(seen).
-author("pizza@parseerror.com").
-export(
	[
		test/0,
		loop/0
	]).

-include_lib("../irc.hrl").
-import(test).
-import(irc).
-import(ircutil).

test() ->
	% TODO: actual, you know, tests.
	true.

loop() ->
	receive
		% seen
		{act, Pid, Irc, Msg, Dst, Nick, ["seen", Who]} ->
			seen(Pid, Irc, Msg, Dst, Nick, Who),
			loop();
		% scan all lines for activity
		{act, Pid, Irc, _, Dst, Nick, Txt} ->
			see(Pid, Irc, Dst, Nick, Txt),
			loop();
		% help
		{help, Pid, Dst, Nick} ->
			Pid ! { q,
				[ irc:resp(Dst, Nick, Nick ++ ": " ++ Txt) || Txt <-
					[
						"[\"seen\", User] -> print when last seen"
					]
				] },
			loop()
	end.

% update seen entry for line of text
see(Pid, Irc, Dst, Nick, Txt) ->
	Seen = irc:state(Irc, seen, dict:new()),
	Txt2 = ircutil:dotdotdot(Txt, 100),
	Txt3 = util:j(Txt2),
	Val = {util:timestamp(),Dst,Txt3},
	%io:format("see(Nick=~s, Val=~p~n", [Nick,Val]),
	Seen2 = dict:store(Nick, Val, Seen),
	Pid ! {setstate, seen, Seen2}.

% look up Nick in seen dict
seen(Pid, Irc, Msg, Dst, _, Who) ->
	Seen = irc:state(Irc, seen, dict:new()),
	Answer = case dict:find(Who, Seen) of
		error ->
			if
				Who == (Irc#ircconn.user)#ircsrc.nick -> "Right here, moron.";
				true -> "Haven't seen " ++ Who
			end;
		{ok, {When, Where, What}} ->
			Foo = lists:flatten(io_lib:format("Last saw ~s at ~s @ ~s saying: ~s",
				[Who,When,Where,What])),
			io:format("dict_get Who=~p Answer=~p~n", [Who, Foo]),
			Foo;
		_ -> nil
	end,
	Pid ! { pipe, Msg, irc:resp(Dst, Who, Answer) }.

