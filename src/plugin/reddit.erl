% ex: set ts=2 noet:
% fetch the reddit programming rss feed and display

% TODO: this is 95% identical to the slashdot plugin.
% extract, abstract rss and tinyurl handling.

-module(reddit).
-export([
		test/0,
		loop/0,
		rss/5
	]).

-include_lib("../irc.hrl").
-import(irc).
-import(ircutil).
-import(test).

-define(rss_url,				 "http://reddit.com/r/programming.rss").
-define(timeout,				 5000).
-define(stories_max,     5).
-define(stories_default, 1).

test() ->
	true.

loop() ->
	receive
		{act, Pid, _, Msg, Dst, Nick, ["reddit", Cnt]} ->
			rss(Pid, Msg, Dst, Nick, list_to_integer(Cnt)),
			loop();
		{act, Pid, _, Msg, Dst, Nick, ["reddit"]} ->
			rss(Pid, Msg, Dst, Nick, ?stories_default),
			loop();
		{act, _, _, _, _, _, _} ->
			loop();
		{help, Pid, Dst, Nick} ->
			Pid ! {q,
				irc:resp(Dst, Nick, Nick ++ ": " ++
					"[\"reddit\" | Cnt] -> latest reddit programming stories (Cnt <= 5, default 1).")
			},
			loop()
	end.

rss(Pid, Msg, Dst, Nick, Cnt) ->
	{Code, Content} = content(?rss_url),
	io:format("rss Code=~p Content=...~n", [Code]),
	case Code of
		error ->
			Pid ! {q, irc:resp(Dst, Nick,
				lists:flatten(io_lib:format("reddit -> ~s", [Code])))};
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
					io_lib:format("reddit ~-55s ~s",
						[ ircutil:dotdotdot(cgi:entity_decode(Title), 55), tinyurl(Url)])))
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
	io:format("Tags=~p~n", [Tags]),
	Meat = [ % filter out stuff i don't want
		lists:filter(
			fun(X) ->
				case X of
					{<<"title">>,_,_} -> true;
					{<<"guid">>,_,_} -> true; % url 3rd item
					_ -> false
				end
			end, T)
		|| T <- Tags
	],
	[ % extract the title and url as the 3rd item in their respective tuples, convert to string
		[ binary_to_list(hd(Guts)) || {_,_,Guts} <- M ]
		|| M <- Meat
	].

%Tags=[[{<<"title">>,[],
%        [<<"Urinal protocol vulnerability (from the xkcd guy)">>]},
%       {<<"link">>,[],[]},
%       <<"http://www.reddit.com/r/programming/comments/9gwcj/urinal_protocol_vulnerability_from_the_xkcd_guy/">>,
%       {<<"guid">>,
%        [{<<"isPermaLink">>,<<"true">>}],
%        [<<"http://www.reddit.com/r/programming/comments/9gwcj/urinal_protocol_vulnerability_from_the_xkcd_guy/">>]},
%       {<<"pubDate">>,[],[<<"Thu, 03 Sep 2009 11:12:00 +0000">>]},
%       {<<"dc:date">>,[],[<<"2009-09-03T11:12:00.251464+00:00">>]},
%       {<<"description">>,[],
%        [<<"submitted by <a href=\"http://www.reddit.com/user/madssj\"> madssj </a> <br/> <a href=\"http://blag.xkcd.com/2009/09/02/urinal-protocol-vulnerability/\">[link]</a> <a href=\"http://www.reddit.com/r/programming/comments/9gwcj/urinal_protocol_vulnerability_from_the_xkcd_guy/\">[138 comments]</a>">>]}],
%      [{<<"title">>,[],
%        [<<65,112,112,108,101,32,104,97,115,32,112,117,108,108,101,100,32,97,
%           110,32,97,112,112,32,102,114,111,109,32,116,104,101,105,114,32,
%           115,116,111,114,101,32,116,111,100,97,121,32,98,101,99,97,117,115,
%           101,32,105,116,226,128,153,115,32,99,104,97,116,32,98,117,98,98,
%           108,101,115,32,97,114,101,32,226,128,156,116,111,111,32,115,104,
%           105,110,121,46,226,128,157>>]},
%       {<<"link">>,[],[]},
%       <<"http://www.reddit.com/r/programming/comments/9gquv/apple_has_pulled_an_app_from_their_store_today/">>,
%       {<<"guid">>,
%        [{<<"isPermaLink">>,<<"true">>}],
%        [<<"http://www.reddit.com/r/programming/comments/9gquv/apple_has_pulled_an_app_from_their_store_today/">>]},
%       {<<"pubDate">>,[],[<<"Wed, 02 Sep 2009 23:36:06 +0000">>]},
%       {<<"dc:date">>,[],[<<"2009-09-02T23:36:06.092449+00:00">>]},
%       {<<"description">>,[],
%        [<<"submitted by <a href=\"http://www.reddit.com/user/livejamie\"> livejamie </a> <br/> <a href=\"http://www.techcrunch.com/2009/09/01/developers-be-warned-apple-has-apparently-trademarked-those-shiny-chat-bubbles/\">[link]</a> <a href=\"http://www.reddit.com/r/programming/comments/9gquv/apple_has_pulled_an_app_from_their_store_today/\">[374 comments]</a>">>]}],
%      [{<<"title">>,[],[<<"Code Rot">>]},
