% ex: set ts=2 noet:
% $Id$
% custom bot logic

-module(react).
-author("pizza@parseerror.com").
-export(
	[
		test/0,
		privmsg/2,
		isquestion/1, dequestion/1, stripjunk/1,
		exec/1
	]).

-include_lib("irc.hrl").
-import(test).
-import(char).
-import(irc).
-import(bot).
-import(erl).

% handle a privmsg
privmsg(Irc,
	#ircmsg{
		type="PRIVMSG",
		dst=Dst,
		src=#ircsrc{nick=From},
		txt=[First|Rest]=All,
		rawtxt=Rawtxt
	}=Msg) ->
	Me = (Irc#ircconn.user)#ircsrc.nick,
	if % are you talking to me? i don't see anyone else...
		Dst == Me ->
			act(Irc, Msg, Dst, From, All);
		First == Me ->
			Msg2 = ltrim_nick(Msg, Rawtxt, Me),
			act(Irc, Msg2, Dst, From, Rest);
		true -> Irc
	end.

% if nick has sufficient perms Perm then run func Exec,
% else return Irc
byperm(Irc, Nick, _Perm, Exec) ->
	% TODO: implement real perm groups, etc.
	case Nick == irc:master(Irc) of
		true -> Exec();
		false -> Irc
		end.

% IRC-related command crap
act(Irc, _Msg, _, Nick, ["say", Chan | What]) ->
	byperm(Irc, Nick, ["irc","say"],
		fun() ->
			bot:q(Irc, irc:privmsg(Chan, util:j(What)))
			end);
act(Irc, _Msg, _, Nick, ["action", Chan | What]) ->
	byperm(Irc, Nick, ["irc","action"],
		fun() ->
			bot:q(Irc, irc:privmsg(Chan, irc:action(util:j(What))))
			end);
act(Irc, _Msg, _, Nick, ["join", Chan]) ->
	byperm(Irc, Nick, ["irc","join"],
		fun() ->
			bot:q(Irc, #ircmsg{type="JOIN", rawtxt=Chan})
			end);
act(Irc, _Msg, _, Nick, ["part", Chan]) ->
	byperm(Irc, Nick, ["irc","part"],
		fun() ->
			bot:q(Irc, #ircmsg{type="PART", rawtxt=Chan})
			end);
act(Irc, _Msg, Dst, Nick, ["irc", "msgtypes"]) ->
	byperm(Irc, Nick, ["irc","msgtypes"],
		fun() ->
			St = irc:state(Irc, irctype, dict:new()),
			KV = lists:map(
				fun(K) -> {K, dict:fetch(K, St)} end,
				dict:fetch_keys(St)),
			Sorted = lists:sort(
				fun({_,A},{_,B}) -> A > B end, KV),
			Out = "types:" ++
				lists:foldl(fun({K,Cnt}, Acc) -> Acc ++
					lists:flatten(io_lib:format(" ~s=~p",[K,Cnt])) end,
					"", Sorted),
			bot:q(Irc, irc:resp(Dst, Nick, Out))
			end);
% evaluate the input as erlang 
act(Irc, #ircmsg{rawtxt=Rawtxt}, Dst, Nick, ["erl" | _What]) ->
	Code = string:substr(Rawtxt, length("erl")+1),
	RespLines = exec(Code),
	Resps = % build an ircmsg for each line
		lists:map(fun(Line) -> irc:resp(Dst, Nick, Line) end,
			RespLines),
	bot:q(Irc, Resps);
% dictionary retrieve
act(Irc, _Msg, Dst, Nick, ["what", "is" | Term]) ->
	Is = irc:state(Irc, is, dict:new()),
	JTerm = util:join("", Term),
	RealTerm = util:nth(1, dequestion(stripjunk([JTerm])), ""),
	Answer = 
		case dict:find(RealTerm, Is) of
			error -> "I don't know";
			{ok, X} -> RealTerm ++ " is " ++ X 
		end,
	bot:q(Irc, irc:resp(Dst, Nick, Nick ++ ": " ++ Answer));
% dictionary store
act(Irc, _Msg, Dst, Nick, [Term, "is" | Rest]) ->
	Is = irc:state(Irc, is, dict:new()),
	RealTerm = util:nth(1, stripjunk([Term]), ""),
	Is2 = dict:store(RealTerm, util:j(Rest), Is),
	Irc2 = irc:setstate(Irc, is, Is2),
	bot:q(Irc2,
		irc:resp(Dst, Nick, Nick ++ ": if you say so."));
% hardcoded tribute to George Carlin, plus making fun of
% mod_spox's actually-helpful weather command
act(Irc, _Msg, Dst, Nick, ["weather"]) ->
	bot:q(Irc,
		irc:resp(Dst, Nick,
			Nick ++ ": Tonight's forecast: Dark. " ++
				"Continued dark throughout most of the evening, " ++
				"with some widely-scattered light towards morning."));
% huh?
act(Irc, _Msg, Dst, Nick, _) ->
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
				fun(X) -> char:isprint(X) end, W) end,
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

% execute erlang source code and return [ "results" ]
% TODO: possibly make size of output configurable...
exec(Code) ->
	Res = erl:eval(Code), % eval
	Res2 = util:show(Res), % format
	Lines = util:lines(Res2),
	Lines2 = % limit number of lines
		if
			length(Lines) > 3 -> lists:sublist(Lines, 3) ++ "...";
			true -> Lines
			end,
	StrLines = lists:map(fun(X) -> util:show(X) end, Lines2),
	Out = string:join(StrLines, ""),
	TrimQuotes = string:substr(Out, 2, length(Out)-2),
	Unescaped = util:unescape(TrimQuotes),
	TrimQuotes2 = util:trim(Unescaped, $"),
 	Trimmed = % limit total length
		if
			length(TrimQuotes2) > 250 -> string:substr(TrimQuotes2, 1, 247) ++ "...";
			true -> TrimQuotes2
			end,
	util:lines(Trimmed).

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

