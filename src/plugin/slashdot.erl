% ex: set ts=2 noet:
% fetch the slashdot rss feed and display

-module(slashdot).
-export([
		test/0,
		loop/0,
		rss/4
	]).

-include_lib("../irc.hrl").
-import(irc).
-import(ircutil).
-import(test).

-define(rss_url,				 "http://rss.slashdot.org/Slashdot/slashdot").
-define(timeout,				 5000).
-define(stories_max,     5).
-define(stories_default, 1).

test() ->
	true.

loop() ->
	receive
		{act, Pid, _, _, Dst, Nick, ["slashdot", Cnt]} ->
			rss(Pid, Dst, Nick, list_to_integer(Cnt)),
			loop();
		{act, Pid, _, _, Dst, Nick, ["slashdot"]} ->
			rss(Pid, Dst, Nick, ?stories_default),
			loop();
		{act, _, _, _, _, _, _} ->
			loop();
		{help, Pid, Dst, Nick} ->
			Pid ! {q,
				irc:resp(Dst, Nick, Nick ++ ": " ++
					"[\"slashdot\" | Cnt] -> display latest slashdot rss feed")
			},
			loop()
	end.

rss(Pid, Dst, Nick, Cnt) ->
	{Code, Content} = content(?rss_url),
	io:format("rss Code=~p Content=~p~n",
		[Code, Content]),
	case Code of
		error ->
			Pid ! {q, irc:resp(Dst, Nick,
				lists:flatten(	o_lib:format("slashdot -> ~s", [Code])))};
		_ -> ok(Pid, Dst, Nick, Cnt, Content)
	end.

content(Url) ->
	case http:request(get,{Url,[]},[{timeout, ?timeout}],[]) of
		{ok, {{_Http, Code, _Msg}, _Header, Content}} -> {Code, Content };
		{error, Why} -> {error, Why}
		end.

tinyurl(Url) ->
	Url2 = "http://is.gd/api.php?longurl=" ++ Url,
	{_Code, Content} = content(Url2),
	if
		is_atom(Content) -> Url; % original URL
		true -> Content
	end.

% url fetch succeeded, display items
ok(Pid, Dst, Nick, Cnt, Content) ->
	Cnt2 = util:min(Cnt, ?stories_max),
	Items = rss_items(Content),
	io:format("Cnt2=~p Items=~p~n", [Cnt2, Items]),
	% because tinyurl() could timeout, send the responses one
  % at a time for better interactivity
	[
		Pid ! {q,
			irc:resp(Dst, Nick,
				lists:flatten(
					io_lib:format("~-55s ~s",
						[ ircutil:dotdotdot(Title, 55), tinyurl(Url)])))
		} || [Title,_Descr,Url] <- string:substr(Items, 1, Cnt2)
	].

% given an rss document, return all items in the format:
% [["Title","Descr","Url"]|Rest]
rss_items([]) -> [];
rss_items(Rss) ->
	Doc = mochiweb_html:parse(Rss),
	Mapping = [{<<"link">>,1},{<<"myUrl">>,2}],
	F =
		fun(_Ctx, [String]) ->
			proplists:get_value(String, Mapping, 0) end,
	MyFuns = [{my_fun, F, [string]}],
	Items = mochiweb_xpath:execute("//item", Doc, MyFuns),
	Tags = [ Tags || {_Item, _Attr, Tags} <- Items ],
	Meat = [ % filter out stuff i don't want
		lists:filter(
			fun(X) ->
				case X of
					{<<"title">>,_,_} -> true;
					{<<"description">>,_,_} -> true;
					{<<"feedburner:origLink">>,_,_} -> true;
					_ -> false
				end
			end, T)
		|| T <- Tags
	],
	[
		[ binary_to_list(hd(Guts)) || {_,_,Guts} <- M ]
		|| M <- Meat
	].


