% ex: set ts=2 noet:
% fetch the fark rss feed and display

% TODO: this is 95% identical to the slashdot plugin.
% extract, abstract rss and tinyurl handling.

-module(fark).
-export([
		test/0,
		loop/0,
		rss/5
	]).

-include_lib("../irc.hrl").
-import(irc).
-import(ircutil).
-import(test).

-define(rss_url,				 "http://fark.com/fark.rss").
-define(timeout,				 5000).
-define(stories_max,     5).
-define(stories_default, 1).
-define(title_len,       120). % fark headlines are so fucking long

test() ->
	true.

loop() ->
	receive
		{act, Pid, _, Msg, Dst, Nick, ["fark", Cnt]} ->
			rss(Pid, Msg, Dst, Nick, list_to_integer(Cnt)),
			loop();
		{act, Pid, _, Msg, Dst, Nick, ["fark"]} ->
			rss(Pid, Msg, Dst, Nick, ?stories_default),
			loop();
		{act, _, _, _, _, _, _} ->
			loop();
		{help, Pid, Dst, Nick} ->
			Pid ! {q,
				irc:resp(Dst, Nick, Nick ++ ": " ++
					"[\"fark\" | Cnt] -> latest fark stories (Cnt <= 5, default 1).")
			},
			loop()
	end.

rss(Pid, Msg, Dst, Nick, Cnt) ->
	{Code, Content} = content(?rss_url),
	io:format("rss Code=~p Content=...~n", [Code]),
	case Code of
		error ->
			Pid ! {q, irc:resp(Dst, Nick,
				lists:flatten(io_lib:format("fark -> ~s", [Code])))};
		_ -> ok(Pid, Msg, Dst, Nick, Cnt, Content)
	end.

content(Url) ->
	case http:request(get,{Url,[]},[{timeout, ?timeout}],[]) of
		{ok, {{_Http, Code, _Msg}, _Header, Content}} -> {Code, Content };
		{error, Why} -> {error, Why}
		end.

tinyurl(Url) ->
	Url2 = "http://tinyurl.com/api-create.php?url=" ++ Url,
	{_Code, Content} = content(Url2),
	if
		is_atom(Content) -> Url; % original URL
		true -> Content
	end.

% url fetch succeeded, display items
ok(Pid, Msg, Dst, Nick, Cnt, Content) ->
	Cnt2 = util:min(Cnt, ?stories_max),
	Items = rss_items(Content),
	% because tinyurl() could timeout, send the responses one
  % at a time for better interactivity
	Pid ! {pipe, Msg,
		[
			irc:resp(Dst, Nick,
				lists:flatten(
					% FIXME: fark has really long headlines. should we chop them way down or
					% perhaps do some fancy multiline formatting?
					io_lib:format("[f] ~-" ++ integer_to_list(?title_len) ++ "s ~s",
						[ ircutil:dotdotdot(cgi:entity_decode(Title), ?title_len), tinyurl(Url)])))
			|| [Title,Url] <- string:substr(Items, 1, Cnt2)
		] }.

% given an rss document, return all items in the format:
% [["Title","Url"]|Rest]
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
					{<<"guid">>,_,_} -> true;
					_ -> false
				end
			end, T)
		|| T <- Tags
	],
	[ % extract the title and url as the 3rd item in their respective tuples, convert to string
		[ binary_to_list(hd(Guts)) || {_,_,Guts} <- M ]
		|| M <- Meat
	].

% Tags=[[{<<"title">>,[],
%        [<<"Good news: The state bureau of investigation is going to get you off the hook. Bad news: You're going to Hell. Good news: You're in Georgia, so probably won't be able to tell the difference [Asinine]">>]},
%       {<<"link">>,[],[]},
%       <<"http://www.fark.com/4615407">>,
%       {<<"description">>,[],
%        [<<"<table> <tr><td> <a href=\"http://www.fark.com/4615407\"><img alt=\"Asinine\" title=\"Asinine\" src=\"http://img.fark.net/pub/topics/asinine.gif\" width=\"54\" height=\"11\"></a> </td><td>  <a href=\"http://www.fark.com/4615407\">[link]</a> <a href=\"http://www.fark.com/4615407\">[20 comments]</a> </td></tr></table>">>]},
%       {<<"media:title">>,[],
%        [<<"Good news: The state bureau of investigation is going to get you off the hook. Bad news: You're going to Hell. Good news: You're in Georgia, so probably won't be able to tell the difference [Asinine]">>]},
%       {<<"media:thumbnail">>,
%        [{<<"url">>,<<"http://img.fark.net/pub/topics/asinine.gif">>}],
%        []},
%       {<<"pubDate">>,[],[<<"Thu, 03 Sep 2009 14:22:54 EDT">>]},
%       {<<"guid">>,
%        [{<<"isPermaLink">>,<<"true">>}],
%        [<<"http://www.fark.com/4615407">>]}],
%      [{<<"title">>,[],
%        [<<"100 Year Old color pictures of Russia developed using digital methods [Cool]">>]},
%       {<<"link">>,[],[]},
%       <<"http://www.fark.com/4615292">>,
%       {<<"description">>,[],
%        [<<"<table> <tr><td> <a href=\"http://www.fark.com/4615292\"><img alt=\"Cool\" title=\"Cool\" src=\"http://img.fark.net/pub/topics/cool.gif\" width=\"54\" height=\"11\"></a> </td><td>  <a href=\"http://www.fark.com/4615292\">[link]</a> <a href=\"http://www.fark.com/4615292\">[40 comments]</a> </td></tr></table>">>]},
%       {<<"media:title">>,[],
%        [<<"100 Year Old color pictures of Russia developed using digital methods [Cool]">>]},
%       {<<"media:thumbnail">>,
%        [{<<"url">>,<<"http://img.fark.net/pub/topics/cool.gif">>}],
%        []},


