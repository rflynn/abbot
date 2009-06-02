% ex: set ts=2 noet:
% various methods of insulting someone

-module(insult).
-author("pizza@parseerror.com").
-export(
	[
		test/0,
		loop/0
	]).

-include_lib("../irc.hrl").
-import(test).
-import(irc).

test() ->
	true.

loop() ->
	receive
		{ act, Pid, _Irc, _, Dst, Nick, ["insult"]} ->
			insult(Pid, Dst, Nick, Nick),
			loop();
		{ act, Pid, _Irc, _, Dst, Nick, ["insult", Who]} ->
			insult(Pid, Dst, Nick, Who),
			loop();
		{ act, Pid, _Irc, _, Dst, Nick, ["insult", Who, "mom"]} ->
			yourmom(Pid, Dst, Nick, Who),
			loop();
		{ act, Pid, _Irc, _, Dst, Nick, ["insult", Who, "momma"]} ->
			yourmom(Pid, Dst, Nick, Who),
			loop();
		{ act, Pid, _Irc, _, Dst, Nick, ["your", "mom"]} ->
			yourmom(Pid, Dst, Nick, Nick),
			loop();
		{ act, Pid, _Irc, _, Dst, Nick, ["yo", "momma"]} ->
			yourmom(Pid, Dst, Nick, Nick),
			loop();
		{ act, _, _, _, _, _, _ } ->
			loop();
		{ help, Pid, Dst, Nick } ->
			Pid ! { q,
				irc:resp(Dst, Nick, Nick ++ ": " ++
						"[\"insult\" | Who] -> insult Who") },
			loop()
	end.

yourmom(Pid, Dst, Nick, Who) ->
	insult(Pid, Dst, Nick, Who, " ", "./data/insult/yourmom").

insult(Pid, Dst, Nick, Who) ->
	insult(Pid, Dst, Nick, Who, ": ", "./data/insult/insults").

insult(Pid, Dst, Nick, Who, Connect, Path) ->
	io:format("insult Who=~p Path=~s~n", [Who, Path]),
	Lines = util:readlines(Path),
	Len = length(Lines),
	if
		Len > 0 ->
			{S1, S2, S3} = now(),
			random:seed(S1, S2, S3),
			Line = lists:nth(random:uniform(Len), Lines),
			Line2 = util:rtrim(Line, 10), % trim newline
			Pid ! {q, irc:resp(Dst, Nick, Who ++ Connect ++ Line2)};
		true -> nil
	end.

