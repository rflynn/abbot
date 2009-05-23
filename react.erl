% ex: set ts=2 noet:
% $Id$

-module(react).
-author("pizza@parseerror.com").
-export([privmsg/2]).

-include_lib("irc.hrl").
-import(irc).
-import(bot).

privmsg(Irc, #ircmsg{type="PRIVMSG", dst=Dst, src=#ircsrc{nick=Nick}, txt=[First|Rest]}) ->
	case First == (Irc#ircconn.user)#ircsrc.nick of
	% are you talking to me? i don't see anyone else around here...
		true -> act(Irc, Dst, Nick, Rest);
		false -> Irc
	end.

act(Irc, Dst, Nick, ["what", "is", Term]) ->
	Is = state(Irc, is),
	% locate Term in "is" dict
	Answer = 
		case dict:find(Term, Is) of
			error -> "I don't know";
			{ok, X} -> Term ++ " is " ++ X 
		end,
	bot:queue(Irc,
		irc:respond(Dst, Nick, Nick ++ ": " ++ Answer));

act(Irc, Dst, Nick, [Term, "is" | Rest]) ->
	Is = state(Irc, is),
	Is2 = dict:store(Term, irc:j(Rest), Is),
	State2 = dict:store(is, Is2, Irc#ircconn.state),
	Irc2 = Irc#ircconn{state=State2},
	bot:queue(Irc2,
		irc:respond(Dst, Nick, Nick ++ ": if you say so."));

act(Irc, Dst, Nick, ["weather"]) ->
	bot:queue(Irc,
		irc:respond(Dst, Nick, Nick ++ ": Tonight's forecast: Dark. Continued dark throughout most of the evening, with some widely-scattered light towards morning."));

act(Irc, Dst, Nick, ["what","is","best","in","life?"]) ->
	bot:queue(Irc,
		irc:respond(Dst, Nick, Nick ++ ": To crush your enemies, see them driven before you... and to hear the lamentation of their women!"));

act(Irc, Dst, Nick, _) ->
	bot:queue(Irc, irc:respond(Dst, Nick, Nick ++ ": huh?")).

state(Irc, Key) ->
	% lookup entry in state dict by key
	State = Irc#ircconn.state,
	case dict:find(Key, State) of
		error -> dict:new();
		{ok, X} -> X
	end.

