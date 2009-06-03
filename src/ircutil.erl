% ex: set ts=2 noet:
% $Id$
% generic utilities sensible for an IRC environment

-module(ircutil).
-author("pizza@parseerror.com").
-export(
	[
		test/0,
		isquestion/1,
		dequestion/1,
		stripjunk/1,
		dotdotdot/2,
		ltrim_nick/3
	]).

-include_lib("irc.hrl").
-import(test).
-import(util).

test() ->
	test:unit(ircutil,
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
		{ [ nil ], 				false },
		{ [ 0 ], 					false },
		{ [ "?" ], 				false },
		{ [ [] ], 				false },
		{ [ [""] ], 			false },
		{ [ ["!"] ], 			false },
		{ [ ["?!"] ], 		false },
		{ [ ["?"] ], 			true  },
		{ [ ["??"] ], 		true  },
		{ [ ["???"] ], 		true  },
		{ [ ["a?"] ], 		true  },
		{ [ ["a","b?"] ], true  }
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
		{ [ [] ], [] },
		{ [ [""] ], [""] },
		{ [ ["?"] ], [] },
		{ [ ["!"] ], ["!"] },
		{ [ [[$"]] ], [[$"]] },
		{ [ ["a"] ], ["a"] },
		{ [ ["ab"] ], ["ab"] },
		{ [ ["a?"] ], ["a"] },
		{ [ ["a??"] ], ["a"] },
		{ [ ["c#?"] ], ["c#"] },
		{ [ ["a???"] ], ["a"] },
		{ [ ["a","b"] ], ["a","b"] },
		{ [ ["a","b?"] ], ["a","b"] },
		{ [ ["a?","b?"] ], ["a?","b"] }
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
				fun(X) -> char:isprint(X) end, W) end,
			Words),
	% filter empty words
	lists:filter(fun(W) -> W /= [] end, Strip).

test_stripjunk() ->
	[
		% in  out
		{ [ [] ],     				[]   			},
		{ [ [""] ], 					[""] 			},
		{ [ ["a"] ], 					["a"] 		},
		{ [ ["ab"] ],					["ab"] 		},
		{ [ ["a","b"] ],			["a","b"] },
		{ [ [[1]] ],  				[]  			},
		{ [ ["a\1"] ], 				["a"]			},
		{ [ ["\1a"] ], 				["a"]			},
		{ [ ["\1a\2"] ],			["a"]			},
		{ [ ["a\1","b"] ],		["a","b"]	},
		{ [ ["\1a","b\255"] ],["a","b"]	},
		{ [ [","] ], 					[","] 		},
		{ [ [":"] ], 					[":"] 		}
	].

% remove my name and any trailing punctuation and update Msg.rawtxt
% example: "my_Nick: hello" -> "hello"
ltrim_nick(Msg, Rawtxt, Nick) ->
	Tok = string:tokens(Rawtxt, " :,"),
	Rawtxt2 =
		if
			hd(Tok) == Nick ->
					X1 = string:substr(Rawtxt, length(Nick)+1),
					X2 = util:ltrim(X1, $,),
					X3 = util:ltrim(X2, $:),
					util:ltrim(X3, 32);
			true -> Rawtxt
			end,
	Msg#ircmsg{rawtxt=Rawtxt2}.

% trim a string at a maxlen, show "..." suffix if trimmed
dotdotdot(Str, MaxLen) ->
	if
		length(Str) > MaxLen ->
			util:rtrim(
				string:substr(Str, 1, MaxLen - 3)) ++ "...";
		true ->
			Str
	end.

