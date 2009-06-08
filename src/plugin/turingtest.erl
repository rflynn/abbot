% ex: set ts=2 noet:

% attempt to pass informal Turing tests.

-module(turingtest).
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
	true.

loop() ->
	receive
		% scan all lines for possible queries or definitions
		{act, Pid, Irc, Msg, Dst, Nick, Txt} ->
			behuman(Pid, Irc, Msg, Dst, Nick, Txt),
			loop();
		% help
		{help, Pid, Dst, Nick} ->
			Pid ! { q, 
				[ irc:resp(Dst, Nick, Nick ++ ": " ++ Txt) || Txt <-
					[
						"This module displays my personality. "
						"I keep my friends close and my enemies closer."
					]
				] },
			loop()
	end.

behuman(Pid, Irc, Msg, Dst, Nick, Txt) ->
	MyNick = (Irc#ircconn.user)#ircsrc.nick,
	Txt2 = ircutil:strippunct(ircutil:dequestion(Txt)),
	Resp =
		case isgreeting(Msg, MyNick, Txt2) of
			true -> greet();
			false ->
				if
					Msg#ircmsg.tome ->
						case Txt2 of
							["are","you","a","bot"] -> areyouabot();
							["prove","it"] -> proveit();
							["prove","you","are","not","a","bot"] -> proveit();
							_ -> confused()
						end;
					true -> []
				end
		end,
	if
		[] == Resp -> nil;
		true -> respond(Pid, Dst, Nick, any(Resp))
	end.

isgreeting(#ircmsg{tome=true}, _, [First|_]) ->
	io:format("isgreeting tome First=~p~n", [First]),
	greeting([First]);
isgreeting(#ircmsg{tome=false}, MyNick, [First|_]=Txt) ->
	io:format("isgreeting !tome First=~p~n", [First]),
	(greeting([First]) and (lists:last(Txt) == MyNick));
isgreeting(_, _, Txt) ->
	io:format("isgreeting something else... Txt=~p~n", [Txt]),
	false.

any(List) ->
	{S1, S2, S3} = now(),
	random:seed(S1, S2, S3),
	lists:nth(random:uniform(length(List)), List).

respond(Pid, Dst, Nick, {How,Txt}) ->
	% pause for a random amount of time before speaking,
	% so as to not appear too bot-like
	spawn(
		fun() ->
			{S1, S2, S3} = now(),
			random:seed(S1, S2, S3),
			Wait = (3 + trunc(random:uniform(20))),
			io:format("In ~w seconds I'll ~w \"~s\"...~n",
				[Wait,How,Txt]),
			timer:sleep(Wait * 1000),
			Pid ! { q, irc:resp(Dst, Nick, resp(Nick, How, Txt)) }
		end).

resp(Nick, say, Txt) ->
	{S1, S2, S3} = now(),
	random:seed(S1, S2, S3),
	case random:uniform() < 0.5 of
		true -> Txt;
		false -> Nick ++ ": " ++ Txt
	end;
resp(_, act, Txt) -> irc:action(Txt).

% is this a greeting?
greeting(["hello"])			-> true;
greeting(["greeting"])	-> true;
greeting(["greetings"])	-> true;
greeting(["hi"])				-> true;
greeting(["welcome"])		-> true;
greeting(["afternoon"])	-> true;
greeting(["evening"])		-> true;
greeting(_)							-> false.

greet() ->
	[ % response to a greeting
		{say, "hi" },
		{say, "hello" }
	].

confused() ->
	[ % addresses to use that we don't understand
		{say, "..." },
		{say, "huh?" },
		{say, "what?" },
		{say, "hmm?" },
		{say, "sorry?" },
		{say, "i don't understand" },
		{say, "english isn't my first language" }
	].

areyouabot() ->
	[ % responses to "are you a bot?" or similar
		{say, "are you?" },
		{say, "aren't we all?" },
		{say, "is this a test?" },
		{say, "i don't think so" },
		{say, "no" },
		{say, "not last time i checked" },
		{say, "no, i just work here" }
	].

proveit() ->
	[ % responses to "prove you are not a bot" or similar
		{say, "you first" },
		{say, "after you" },
		{act, "sighs" },
		{act, "rolls eyes" },
		{act, "gets out the Voight-Kampf machine" }
	].

% we could also use "insult" perhaps...

