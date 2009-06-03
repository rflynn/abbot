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
-import(util).

test() ->
	true.

loop() ->
	receive
		{ act, Pid, _Irc, Msg, Dst, Nick, ["insult"]} ->
			insult(Pid, Msg, Dst, Nick, Nick),
			loop();
		{ act, Pid, _Irc, Msg, Dst, Nick, ["insult", Who]} ->
			insult(Pid, Msg, Dst, Nick, Who),
			loop();
		{ act, Pid, _Irc, Msg, Dst, Nick, ["insult", Who, "mom"]} ->
			yourmom(Pid, Msg, Dst, Nick, Who),
			loop();
		{ act, Pid, _Irc, Msg, Dst, Nick, ["insult", Who, "momma"]} ->
			yourmom(Pid, Msg, Dst, Nick, Who),
			loop();
		{ act, Pid, _Irc, Msg, Dst, Nick, ["your", "mom"]} ->
			yourmom(Pid, Msg, Dst, Nick, Nick),
			loop();
		{ act, Pid, _Irc, Msg, Dst, Nick, ["yo", "momma"]} ->
			yourmom(Pid, Msg, Dst, Nick, Nick),
			loop();
		{ act, _, _, _, _, _, _ } ->
			loop();
		{ help, Pid, Dst, Nick } ->
			Pid ! { q,
				[ irc:resp(Dst, Nick, Nick ++ ": " ++ Txt) || Txt <-
						[ "[\"insult\"] -> insult yourself",
							"[\"insult\", Who] -> insult Who",
							"[\"insult\", Who, \"mom\"] -> insult Who's momma",
							"[\"insult\", Who, \"momma\"] -> insult Who's momma"
						]
				] },
			loop()
	end.

yourmom(Pid, Msg, Dst, Nick, Who) ->
	insult(Pid, Msg, Dst, Nick, Who, " ",
		util:relpath("insult.erl", "data/insult/yourmom")).

insult(Pid, Msg, Dst, Nick, Who) ->
	insult(Pid, Msg, Dst, Nick, Who, ": ",
		util:relpath("insult.erl", "data/insult/insults")).

insult(Pid, Msg, Dst, Nick, Who, Connect, Path) ->
	io:format("insult Who=~p Path=~s~n", [Who, Path]),
	Lines = util:readlines(Path),
	Len = length(Lines),
	if
		Len > 0 ->
			{S1, S2, S3} = now(),
			random:seed(S1, S2, S3),
			Line = lists:nth(random:uniform(Len), Lines),
			Line2 = util:rtrim(Line, 10), % trim newline
			Pid ! {pipe, Msg, irc:resp(Dst, Nick, Who ++ Connect ++ Line2)};
		true -> nil
	end.

