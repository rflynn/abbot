% ex: set ts=2 noet:
% $Id$
% custom bot logic

-module(react).
-author("pizza@parseerror.com").
-export(
	[
		test/0,
		privmsg/2,
		exec/1
	]).

-include_lib("irc.hrl").
-import(test).
-import(char).
-import(irc).
-import(ircutil).
-import(bot).
-import(erl).
-import(ruby).

test() ->
	true.

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
act(Irc, _Msg, _, Nick, ["nick", NewNick]) ->
	byperm(Irc, Nick, ["irc","nick"],
		fun() ->
			NewUser = (Irc#ircconn.user)#ircsrc{nick=NewNick},
			Irc2 = Irc#ircconn{user=NewUser},
			bot:nick(Irc2)
			end);
act(Irc, _Msg, _, Nick, ["quit" | Snarky]) ->
	byperm(Irc, Nick, ["irc","quit"],
		fun() ->
			bot:q(Irc, #ircmsg{type="QUIT", rawtxt=util:j(Snarky)})
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

% dict get
act(Irc, _Msg, Dst, Nick, ["what", "is" | Term]) ->
	dict_get(Irc, Dst, Nick, "is", Term);
act(Irc, _Msg, Dst, Nick, ["what", "are" | Term]) ->
	dict_get(Irc, Dst, Nick, "are", Term);
% dict store
act(Irc, _Msg, Dst, Nick, [Term, "is" | Rest]) ->
	dict_set(Irc, Dst, Nick, Term, Rest);
act(Irc, _Msg, Dst, Nick, [Term, "are" | Rest]) ->
	dict_set(Irc, Dst, Nick, Term, Rest);
% dict forget
act(Irc, _Msg, Dst, Nick, ["forget" | Term]) ->
	Is = irc:state(Irc, is, dict:new()),
	Term2 = dict_term(Term),
	io:format("dict_forget Term=~p Term2=~p~n", [Term, Term2]),
	Is2 = dict:erase(Term2, Is),
	Irc2 = irc:setstate(Irc, is, Is2),
	bot:q(Irc2,
		irc:resp(Dst, Nick, Nick ++ ": forgotten."));

% evaluate the input as erlang 
act(Irc, #ircmsg{rawtxt=Rawtxt}, Dst, Nick, ["erl" | _What]) ->
	Code = string:substr(Rawtxt, length("erl")+1),
	RespLines = exec(Code),
	Resps = % build an ircmsg for each line
		lists:map(fun(Line) -> irc:resp(Dst, Nick, Line) end,
			RespLines),
	bot:q(Irc, Resps);

% evaluate the input as erlang
act(Irc, _, Dst, Nick, ["ruby" | Code]) ->
	Source = util:j(Code),
	Output = ruby:eval(Source), % we get at most one line of output
	bot:q(Irc, irc:resp(Dst, Nick, Output));

% hardcoded tribute to George Carlin, plus making fun of
% mod_spox's actually-helpful weather command
act(Irc, _Msg, Dst, Nick, ["weather"]) ->
	bot:q(Irc,
		irc:resp(Dst, Nick,
			Nick ++ ": Tonight's forecast: Dark. " ++
				"Continued dark throughout most of the evening, " ++
				"with some widely-scattered light towards morning."));
act(Irc, _Msg, Dst, Nick, ["quote", Someone]) ->
	PathSafe = % verify path contains no funny business
		lists:all(fun(C)->char:isalnum(C) end, Someone),
	if
		PathSafe ->
			Path = "quote/" ++ Someone,
			Quotes = util:readlines(Path),
			if
				length(Quotes) > 0 ->
					Quote = util:rtrim( % trim newline
						lists:nth(random:uniform(length(Quotes)), Quotes), 10),
					bot:q(Irc, irc:resp(Dst, Nick, Quote));
				true -> Irc
			end;
		true -> Irc
	end;
act(Irc, _Msg, Dst, Nick, ["help"]) ->
	bot:q(Irc, irc:resp(Dst, Nick, Nick ++ ": [" ++
		"{Foo, \"is\" | Def}, " ++
		"{\"what\", \"is\", Foo}, " ++
		"{\"erl\", Code}, " ++
		"{\"irc\", \"msgtypes\"}, " ++
		"{\"weather\"}" ++
		"]"));
% huh?
act(Irc, _Msg, Dst, Nick, _) ->
	bot:q(Irc, irc:resp(Dst, Nick, Nick ++ ": huh?")).

dict_set(Irc, Dst, Nick, "you", Rest) ->
	dict_set(Irc, Dst, Nick, "i", Rest);
dict_set(Irc, Dst, Nick, Term, Rest) ->
	Is = irc:state(Irc, is, dict:new()),
	Term2 = dict_term(Term),
	Val = util:j(ircutil:stripjunk(Rest)),
	io:format("dict_get Term2=~p Val=~p~n", [Term2, Val]),
	Is2 = dict:store(Term2, Val, Is),
	Irc2 = irc:setstate(Irc, is, Is2),
	bot:q(Irc2,
		irc:resp(Dst, Nick, Nick ++ ": if you say so.")).

dict_get(Irc, Dst, Nick, _Connect, ["you?"]) ->
	dict_get(Irc, Dst, Nick, "am", ["i"]);
dict_get(Irc, Dst, Nick, _Connect, ["you"]) ->
	dict_get(Irc, Dst, Nick, "am", ["i"]);
dict_get(Irc, Dst, Nick, Connect, Term) ->
	Is = irc:state(Irc, is, dict:new()),
	Term2 = dict_term(Term),
	Answer = 
		case dict:find(Term2, Is) of
			error -> "I don't know";
			{ok, X} -> Term2 ++ " " ++ Connect ++ " " ++ X 
		end,
	io:format("dict_get Term2=~p Answer=~p~n", [Term2, Answer]),
	bot:q(Irc, irc:resp(Dst, Nick, Nick ++ ": " ++ Answer)).

dict_term(Term) ->
	J = util:join("", Term),
	Strip = ircutil:stripjunk([J]),
	Deq = ircutil:dequestion(Strip),
	util:nth(1, Deq, "").

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

