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
		{ act, Pid, Irc, Msg, Dst, Nick, ["insult"]} ->
			insult(Pid, Irc, Msg, Dst, Nick, ""),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["insult", Who]} ->
			insult(Pid, Irc, Msg, Dst, Nick, Who),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["insult", Who, "mom"]} ->
			yourmom(Pid, Irc, Msg, Dst, Nick, Who),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["insult", Who, "momma"]} ->
			yourmom(Pid, Irc, Msg, Dst, Nick, Who),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["your", "mom"]} ->
			yourmom(Pid, Irc, Msg, Dst, Nick, Nick),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["yo", "momma"]} ->
			yourmom(Pid, Irc, Msg, Dst, Nick, Nick),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["retort"]} ->
			insult(Pid, Irc, Msg, Dst, Nick, ""),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["retort", Who]} ->
			insult(Pid, Irc, Msg, Dst, Nick, Who),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["counter-retort"]} ->
			insult(Pid, Irc, Msg, Dst, Nick, ""),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["counter-retort", Who]} ->
			insult(Pid, Irc, Msg, Dst, Nick, Who),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["riposte"]} ->
			insult(Pid, Irc, Msg, Dst, Nick, ""),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["riposte", Who]} ->
			insult(Pid, Irc, Msg, Dst, Nick, Who),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["addon-riposte"]} ->
			insult(Pid, Irc, Msg, Dst, Nick, ""),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["addon-riposte", Who]} ->
			insult(Pid, Irc, Msg, Dst, Nick, Who),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["apologize"]} ->
			apologize(Pid, Irc, Msg, Dst, Nick, ""),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["apologize", Who]} ->
			apologize(Pid, Irc, Msg, Dst, Nick, Who),
			loop();
		{ act, _, _, _, _, _, _ } ->
			loop();
		{ help, Pid, Dst, Nick } ->
			Pid ! { q,
				[ irc:resp(Dst, Nick, Nick ++ ": " ++ Txt) || Txt <-
						[	"[Cmd | Who] -> direct Cmd at Who",
							"Cmd = [insult,retort,counter-retort,riposte,addon-riposte,apologize]"
						]
				] },
			loop()
	end.

insult(Pid, Irc, Msg, Dst, Nick, "") ->
	Target = target(Irc, Nick),
	insult(Pid, Irc, Msg, Dst, Nick, Target, connect(Target),
		util:relpath("insult.erl", "data/insult/insults"));
insult(Pid, Irc, Msg, Dst, Nick, Who) ->
	insult(Pid, Irc, Msg, Dst, Nick, Who, connect(Who),
		util:relpath("insult.erl", "data/insult/insults")).

apologize(Pid, Irc, Msg, Dst, Nick, "") ->
	Target = target(Irc, Nick),
	insult(Pid, Irc, Msg, Dst, Nick, Target, connect(Target),
		util:relpath("insult.erl", "data/insult/apologies"));
apologize(Pid, Irc, Msg, Dst, Nick, Who) ->
	insult(Pid, Irc, Msg, Dst, Nick, Who, connect(Who),
		util:relpath("insult.erl", "data/insult/apologies")).

yourmom(Pid, Irc, Msg, Dst, Nick, Who) ->
	insult(Pid, Irc, Msg, Dst, Nick, Who, " ",
		util:relpath("insult.erl", "data/insult/yourmom")).

target(Irc, Nick) ->
	Last = irc:state(Irc, insult_last, dict:new()),
	case dict:find(Nick, Last) of
		error -> "";
		{ok, SomeFucker} -> SomeFucker
	end.

connect(Who) ->
	if
		"" == Who -> "";
     true -> ": "
	end.

insult(Pid, Irc, Msg, Dst, Nick, Who, Connect, Path) ->
	io:format("insult Who=~p Path=~s~n", [Who, Path]),
	Lines = util:readlines(Path),
	Len = length(Lines),
	if
		Len > 0 ->
			{S1, S2, S3} = now(),
			random:seed(S1, S2, S3),
			Line = lists:nth(random:uniform(Len), Lines),
			Line2 = util:rtrim(Line, 10), % trim newline
			Last = irc:state(Irc, insult_last, dict:new()),
			Last2 = dict:store(Who, Nick, Last),
			Pid ! {setstate, insult_last, Last2},
			Pid ! {pipe, Msg, irc:resp(Dst, Nick, Who ++ Connect ++ Line2)};
		true -> nil
	end.

