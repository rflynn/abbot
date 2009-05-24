% ex: set ts=2 noet:
% $Id$

-module(react).
-author("pizza@parseerror.com").
-export([privmsg/2, test/0]).
-export([isquestion/1, dequestion/1, stripjunk/1]).

-include_lib("irc.hrl").
-import(test).
-import(ctype).
-import(irc).
-import(bot).

privmsg(Irc, #ircmsg{type="PRIVMSG", dst=Dst, src=#ircsrc{nick=Nick}, txt=[First|Rest]}) ->
	case First == (Irc#ircconn.user)#ircsrc.nick of
		% are you talking to me? i don't see anyone else around here...
		true -> act(Irc, Dst, Nick, Rest);
		false -> Irc
	end.

act(Irc, Dst, Nick, ["what", "is", Term]) ->
	Is = irc:state(Irc, is, dict:new()),
	RealTerm = hd(dequestion(stripjunk([Term]))),
	Answer = 
		case dict:find(RealTerm, Is) of
			error -> "I don't know";
			{ok, X} -> RealTerm ++ " is " ++ X 
		end,
	bot:q(Irc, irc:resp(Dst, Nick, Nick ++ ": " ++ Answer));

act(Irc, Dst, Nick, [Term, "is" | Rest]) ->
	Is = irc:state(Irc, is, dict:new()),
	RealTerm = hd(stripjunk([Term])),
	Is2 = dict:store(RealTerm, util:j(Rest), Is),
	State2 = dict:store(is, Is2, Irc#ircconn.state),
	Irc2 = Irc#ircconn{state=State2},
	bot:q(Irc2,
		irc:resp(Dst, Nick, Nick ++ ": if you say so."));

act(Irc, Dst, Nick, ["weather"]) ->
	bot:q(Irc,
		irc:resp(Dst, Nick,
			Nick ++ ": Tonight's forecast: Dark. " ++
				"Continued dark throughout most of the evening, " ++
				"with some widely-scattered light towards morning."));

act(Irc, Dst, Nick, ["what","is","best","in","life?"]) ->
	bot:q(Irc,
		irc:resp(Dst, Nick,
			Nick ++ ": To crush your enemies, see them driven before you... " ++
				"and to hear the lamentation of their women!"));

act(Irc, Dst, Nick, _) ->
	bot:q(Irc, irc:resp(Dst, Nick, Nick ++ ": huh?")).

test() ->
	test:unit(react,
		[
			{ isquestion, test_isquestion() },
			{ dequestion,	test_dequestion()	},
			{ stripjunk,  test_stripjunk()  }
		]).

% is the input a question?
isquestion([[]]) ->
	false;
isquestion([H|_]=Words) when is_list(H)  ->
	$? == lists:last(lists:last(Words));
isquestion(_) ->
	false.

test_isquestion() ->
	[
		{ nil, 				false },
		{ 0, 					false },
		{ "?", 				false },
		{ [], 				false },
		{ [""], 			false },
		{ ["!"], 			false },
		{ ["?!"], 		false },
		{ ["?"], 			true  },
		{ ["??"], 		true  },
		{ ["???"], 		true  },
		{ ["a?"], 		true  },
		{ ["a","b?"], true  }
	].

% remove trailing "?" from wordlist
dequestion([]) ->
	[];
dequestion([[]]) ->
	[[]];
dequestion(Words) when is_list(Words) ->
	LastWord = lists:last(Words),
	Rest = lists:sublist(Words, length(Words)-1),
	Trimmed = util:rtrim(LastWord, $?),
	case Trimmed of
		"" -> dequestion(Rest);
		_ -> Rest ++ [ Trimmed ]
		end.

test_dequestion() ->
	[
		% in  out
		{ [], [] },
		{ [""], [""] },
		{ ["?"], [] },
		{ ["!"], ["!"] },
		{ [[$"]], [[$"]] },
		{ ["a"], ["a"] },
		{ ["ab"], ["ab"] },
		{ ["a?"], ["a"] },
		{ ["a??"], ["a"] },
		{ ["c#?"], ["c#"] },
		{ ["a???"], ["a"] },
		{ ["a","b"], ["a","b"] },
		{ ["a","b?"], ["a","b"] },
		{ ["a?","b?"], ["a?","b"] }
	].

% remove all unprintable chars from all words
% in a wordlist; and remove any words that consisted
% entirely of them
stripjunk([]) -> [];
stripjunk([[]]) -> [[]];
stripjunk([H|_]=Words) when is_list(H) ->
	% filter junk chars
	Strip =
		lists:map(
			fun(W) -> lists:filter(
				fun(X) -> ctype:isprint(X) end, W) end,
			Words),
	% filter empty words
	lists:filter(fun(W) -> W /= [] end, Strip).

test_stripjunk() ->
	[
		% in  out
		{ [],     				[]   			},
		{ [""], 					[""] 			},
		{ ["a"], 					["a"] 		},
		{ ["ab"],					["ab"] 		},
		{ ["a","b"],			["a","b"] },
		{ [[1]],  				[]  			},
		{ ["a\1"], 				["a"]			},
		{ ["\1a"], 				["a"]			},
		{ ["\1a\2"],			["a"]			},
		{ ["a\1","b"],		["a","b"]	},
		{ ["\1a","b\255"],["a","b"]	}
	].

