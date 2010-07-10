% ex: set ts=2 noet:
%  

-module(quote).
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

quote_path() ->
	quote_path("").
quote_path(File) ->
	Where = util:relpath("quote.erl", "data/quote/") ++ File,
	io:format("quote_path(~p) -> ~p~n", [File, Where]),
	Where.

loop() ->
	receive
		{ act, Pid, Irc, _Msg, Dst, Nick, ["quote" | _]=Txt} ->
			quote(Pid, Irc, _Msg, Dst, Nick, Txt),
			loop();
		{ act, _, _, _, _, _, _ } ->
			loop();
		{ help, Pid, Dst, Nick } ->
			help(Pid, Dst, Nick),
			loop()
	end.

% random quote, random topic
quote(Pid, Irc, _Msg, Dst, Nick, ["quote"]) ->
	Who = quotefiles(),
	{_, _, S3} = now(),
	random:seed(S3, S3, S3),
	Topic = lists:nth(random:uniform(length(Who)), Who),
	quote(Pid, Irc, _Msg, Dst, Nick, ["quote", Topic]);

% random quote, certain topic
quote(Pid, Irc, Msg, Dst, Nick, ["quote", Who | Search]) ->
	Search2 = util:j(Search),
	Someone = Who,
	io:format("quote Someone=~p Search2=~p~n", [Someone, Search2]),
	PathSafe = % verify path contains no funny business
		lists:all(
			fun(C)->
				char:isalnum(C) or char:isspace(C)
			end, Someone),
	if
		not PathSafe -> Irc;
		PathSafe ->
			Path = quote_path(Someone),
			Quotes = lists:filter(
				fun(Q) ->
					("" == Search2) or (string:str(Q, Search2) /= 0)
				end,
				util:readlines(Path)),
			if
				0 == length(Quotes) ->
					Pid ! {q, irc:resp(Dst, Nick, "Quote not found.")};
				true ->
					{S1, S2, S3} = now(),
					random:seed(S1, S2, S3),
					Quote = util:rtrim( % trim newline
						lists:nth(random:uniform(length(Quotes)), Quotes), 10),
					Pid ! {pipe, Msg, irc:resp(Dst, Nick, Quote)}
			end
	end.

% list quotefiles
quotefiles() ->
	case file:list_dir(quote_path()) of
		{ok, List} ->
			lists:filter( % disregard dot-files, sort
				fun(C) ->
					([] /= C) and (lists:nth(1, C) /= $.) end,
				lists:sort(List));
		{error, _} -> []
	end.

help(Pid, Dst, Nick) ->
	Who = quotefiles(),
	Pid ! { q,
		[ irc:resp(Dst, Nick, Nick ++ ": " ++ Txt) || Txt <-
			[
				"[\"quote\"]               -> random quote, random topic",
				"[\"quote\", Who | Search] -> random quote from Who, optionally containing Search",
				"Who = [" ++ util:join(",", Who) ++ "]"
			]
		] }.

