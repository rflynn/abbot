% ex: set ts=2 noet:

% a "dictionary" plugin that remembers what people say and
% answers their questions, occasionally with weird, funny
% or even relevant results

%
% Conceptual Interface:
%
%		i am ashamed 			-> set(FooNick, "ashamed")
%		wings are 50% off!-> set("wings", "50% off!")
%		you are a jerk 		-> set(MyNick, "a jerk")
%
%   foo?         			-> get("foo")
%		what is foo  			-> get("foo")
%		what is foo? 			-> get("foo")
%		who are you? 			-> get(MyNick)
%   what am i?   			-> get(FooNick)
%   wtf is this? 			-> get("this")
%   are you sure? 		-> get("you sure")
%		where is FooNick? -> get("FooNick")
%

-module(def).
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
		% dict forget
		{act, Pid, Irc, _, Dst, Nick, ["forget" | Term]} ->
			dict_forget(Pid, Irc, Dst, Nick, Term),
			loop();
		% def list
		{act, Pid, Irc, Msg, Dst, Nick, ["def", "list"]} ->
			dict_list(Pid, Irc, Msg, Dst, Nick),
			loop();
		{act, Pid, Irc, Msg, Dst, Nick, ["what", "do", "you", "know"]} ->
			dict_list(Pid, Irc, Msg, Dst, Nick),
			loop();
		{act, Pid, Irc, Msg, Dst, Nick, ["what", "do", "you", "know?"]} ->
			dict_list(Pid, Irc, Msg, Dst, Nick),
			loop();
		% scan all lines for possible queries or definitions
		{act, Pid, Irc, Msg, Dst, Nick, Txt} ->
			multiword(Pid, Irc, Msg, Dst, Nick, Txt),
			loop();
		% help
		{help, Pid, Dst, Nick} ->
			Pid ! { pipe, 
				[ irc:resp(Dst, Nick, Nick ++ ": " ++ Txt) || Txt <-
					[
						"[Term, \"is\" | Def]   -> remember Term => Def",
						"[\"what\", \"is\", Term] -> look up Term",
						"[\"def\", \"list\"]      -> Terms i know"
					]
				] },
			loop()
	end.

multiword(_Pid, _Irc, _Msg, _Dst, _Nick, []) -> nil;
multiword(Pid, Irc, Msg, Dst, Nick, [First,Second|Rest]=Txt) ->
	case ircutil:isquestion(Txt) of
		true -> % ends in a question mark
			{D,Connect,Term}=termdef(ircutil:dequestion(Txt)),
			question(Pid, Irc, Msg, Dst, Nick, {D,Connect,Term});
		false ->
			case qword(First) of
				true -> % starts with a question-type word
					question(Pid, Irc, Msg, Dst, Nick, {First,Second,Rest});
				false ->
					case termdef(Txt) of
						{_,_,[]} -> 0;
						{[],Connect,Def} -> question(Pid, Irc, Msg, Dst, Nick, {[], Connect, Def});
			  		Foo -> answer(Pid, Irc, Msg, Dst, Nick, Foo) 
					end
			end
	end;
multiword(Pid, Irc, Msg, Dst, Nick, Txt) -> % too short, append
	case ircutil:isquestion(Txt) of
		true -> % ends in a question mark
			{D,Connect,Term}=termdef(ircutil:dequestion(Txt)),
			question(Pid, Irc, Msg, Dst, Nick, {D,Connect,Term});
		false -> nil
	end.

% does a question begin with this word?
qword("why")	-> true;
qword("what")	-> true;
qword("how")	-> true;
qword("who")	-> true;
qword("are")	-> true;
qword("am")		-> true;
qword("is")		-> true;
qword("wtf")	-> true;
qword(_)			-> false.

% is this a connection between a term and a def, or a leading question word?
connector("are")	-> true;
connector("is")		-> true;
connector("am")		-> true;
connector(_)			-> false.

termdef(Txt) ->
	{Term,Def} =
		lists:splitwith(
			fun(Word) -> not connector(Word) end, Txt),
	{Connect,Def2} =
		if
			Def == [] ->
				{"",""};
			true ->
				[H|T] = Def,
				{H,T}
		end,
	io:format("termdef(~p) -> {~p,~p,~p}~n", [Txt,Term,Connect,Def2]),
	{Term,Connect,Def2}.

question(Pid, Irc, Msg, Dst, Nick, {Def, [], []}) ->
	io:format("question(Def=~p)~n", [Def]),
	dict_get(Pid, Irc, Msg, Dst, Nick, "is", Def);
question(Pid, Irc, Msg, Dst, Nick, {_, Connect, Txt}) ->
	io:format("question(Connect=~p, Txt=~p)~n", [Connect, Txt]),
	dict_get(Pid, Irc, Msg, Dst, Nick, Connect, Txt).

answer(_Pid, _Irc, _Msg, _Dst, _Nick, {[],_,_}) -> nil;
answer(_Pid, _Irc, _Msg, _Dst, _Nick, {_,_,[]}) -> nil;
answer(Pid, Irc, _Msg, _Dst, Nick, {Term,_,Def}) ->
	if
		length(Term) =< 3 ->
			io:format("answer(Term=~p, Def=~p)~n", [Term, Def]),
			dict_set(Pid, Irc, Nick, Term, Def);
		true ->
			nil
	end.

% store Key -> Val mapping in dictionary
% NOTE: translations:
%		"you" -> "i" ("what are you?" -> "i am ...")
%		"i" -> Nick ("i am tired" -> Nick ++ " is tired")
dict_set(_Pid, _Irc, _Nick, ["Karma" | _], _Rest) ->
	nil; % avoid mod_spox's karma reports(!)
dict_set(Pid, Irc, _Nick, ["you"], Rest) ->
	dict_set_(Pid, Irc, ["i"], Rest);
dict_set(Pid, Irc, Nick, ["i"], Rest) ->
	dict_set_(Pid, Irc, [Nick], Rest);
dict_set(Pid, Irc, _Nick, Term, Rest) ->
	dict_set_(Pid, Irc, Term, Rest).

dict_set_(Pid, Irc, Term, Rest) ->
	io:format("dict_set_(Term=~p, Rest=~p)~n", [Term,Rest]),
	Is = irc:state(Irc, is, dict:new()),
	Term2 = util:j(Term),
	Val = ircutil:dotdotdot(util:j(Rest), 100),
	io:format("dict_set Term2=~p Val=~p~n", [Term2, Val]),
	Is2 = dict:store(string:to_lower(Term2), Val, Is),
	Pid ! {setstate, is, Is2}.

% retrieve Key -> Val mapping in dictionary
% NOTE: translate between "you" <-> "i", i.e. "what are you?" -> "i am ..."
dict_get(Pid, Irc, Msg, Dst, Nick, _Connect, ["i"]) ->
	dict_get(Pid, Irc, Msg, Dst, Nick, "is", [Nick]);
dict_get(Pid, Irc, Msg, Dst, Nick, _Connect, ["you"]) ->
	dict_get(Pid, Irc, Msg, Dst, Nick, "am", ["i"]);
dict_get(Pid, Irc, Msg, Dst, Nick, _Connect, Term) ->
	Is = irc:state(Irc, is, dict:new()),
	Term2 = util:j(Term),
	case dict:find(string:to_lower(Term2), Is) of
		error -> nil;
		{ok, X} ->
			Answer = X,
			io:format("dict_get Term2=~p Answer=~p~n",
				[Term2, Answer]),
			Pid ! { pipe, Msg, irc:resp(Dst, Nick, Nick ++ ": " ++ Answer) }
	end.

% 
dict_forget(Pid, Irc, Dst, Nick, Term) ->
	Is = irc:state(Irc, is, dict:new()),
	Term2 = util:j(Term),
	io:format("dict_forget Term=~p Term2=~p~n", [Term, Term2]),
	Is2 = dict:erase(string:to_lower(Term2), Is),
	Pid ! {setstate, is, Is2},
	Pid ! {q, irc:resp(Dst, Nick, Nick ++ ": forgotten.") }.

% list all the topics i know about
dict_list(Pid, Irc, Msg, Dst, Nick) ->
	Is = irc:state(Irc, is, dict:new()),
	Answer = util:join(",", [ K || {K,_} <- lists:sort(dict:to_list(Is)) ]),
	io:format("dict_list=~s~n", [Answer]),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Answer) }.

