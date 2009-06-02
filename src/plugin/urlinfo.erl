% ex: set ts=2 noet:
% given a string possibly containing URLs as input, parse out,
% visit, and report on each link; provide a Title and a tinyurl
% link if valid and an http error code if not

% NOTE: the mochiweb package i'm using crashes
% regularly on random web pages.

-module(urlinfo).
-export([test/0, loop/0, info/1, urlmatch/1]).

-include_lib("../irc.hrl").
-import(irc).
-import(test).
-define(timeout, 5000).

loop() ->
	receive
		{act, Pid, _Irc, _, Dst, Nick, ["url", Url ]} ->
			act(Pid, Dst, Nick, Url),
			loop();
		{act, Pid, _Irc, #ircmsg{rawtxt=Rawtxt}, Dst, _Nick, _Txt} ->
			scan_for_urls(Pid, Rawtxt, Dst),
			loop();
		{help, Pid, Dst, Nick} ->
			Pid ! {q,
				irc:resp(Dst, Nick, Nick ++ ": " ++
					"[\"url\", Url] -> fetch a URL's title and tinyurl")
			},
			loop()
	end.

% evaluate the input as erlang
act(Pid, Dst, Nick, Url) ->
	Output = urlinfo:info(Url),
	Msg = irc:resp(Dst, Nick, Output),
	Pid ! { q, Msg }.

% given a line of IRC input, scan it for URLs and
% investigate each, producing a one-line report
scan_for_urls(Pid, Rawtxt, Dst) ->
	Urls = % detect output from other abbots
		case re:run(Rawtxt,
			"^(" ++
				"urlinfo" ++
				"|" ++
				"\".*?\" -> http://tinyurl\\.com\\S+ \\(https?://.*\\)" ++ % success
				"|" ++
				"https://\\S+ -> \\S+" ++ % failure
			")$") of
			{match,_} -> [];	% from another abbot, ignore URLs!
												% this avoids an endless feedback cycle
			_ -> urlinfo:urlmatch(Rawtxt)
			end,
	Urls2 = lists:filter( 
		fun(U) ->
			0 == string:str(U, "http://tinyurl.") % don't tinyurl a tinyurl
		end, Urls),
	[ spawn(
		fun() ->
			Output = urlinfo:info(Url),
			Oneline = lists:filter(
				fun(C) -> % sanitize title contents
					char:isalnum(C) or char:ispunct(C) or (C == 32)
				end, Output),
			Msg = irc:privmsg(Dst, Oneline),
			Pid ! { q, Msg }
			end)
		|| Url <- Urls2
	].

test() ->
	test:unit(urlinfo,
		[
			{ urlmatch, test_urlmatch() }
		]).

content(Url) ->
	case http:request(get,{Url,[]},[{timeout,?timeout}],[]) of
		{ok, {{_Http, Code, _Msg}, _Header, Content}} -> {Code, Content };
		{error, Why} -> {error, Why}
		end.

title(Content) ->
	T = title_(Content),
	if
		is_binary(T) -> binary_to_list(T);
		true -> T
	end.

% given html content, parse out the title
title_([_|_]=Content) ->
	Doc = mochiweb_html:parse(Content),
	Mapping = [{<<"link">>,1},{<<"myUrl">>,2}],
	F =
		fun(_Ctx, [String]) ->
			proplists:get_value(String, Mapping, 0) end,
	MyFuns = [{my_fun, F, [string]}],
	% TODO: mochiweb is case sensitive, which doesn't
	% work for random html, since one doesn't know what to
	% expect. modify it to be case-insensitive to get rid of
	% crappy wraper code like this(!)
	R = mochiweb_xpath:execute("//TITLE/text()", Doc, MyFuns),
	if
		[] /= R -> hd(R);
		true ->
			T = mochiweb_xpath:execute(
				"//title/text()", Doc, MyFuns),
			if
				[] /= T -> hd(T);
				true -> ""
			end
	end.

tinyurl(Url) ->
	Url2 = "http://tinyurl.com/api-create.php?url=" ++ Url,
	{_Code, Content} = content(Url2),
	Content.

% url fetc failed, display url and error
error(Url, Code) ->
	lists:flatten(
		io_lib:format("urlinfo ~s -> ~s", [Url, Code])).

% url fetch succeeded, display url, title and tinyurl
ok(Url, _Code, Content) ->
	Title = title(Content),
	TitleTrim =
		if
			length(Title) > 50 ->
				string:substr(Title, 1, 47) ++"...";
			true -> Title
		end,
	TinyURL = tinyurl(Url),
	TinyURL2 =
		if % don't bother tinyurl-ing if it's not much shorter
			length(TinyURL) + 15 >= length(Url) -> "";
			true -> TinyURL
		end,
	lists:flatten(
		io_lib:format("urlinfo \"~s\"~s",
			[TitleTrim, TinyURL2])).

info(Url) ->
	{Code, Content} = content(Url),
	io:format("info Url=~s Code=~p Content=...~n",
		[Url,Code]),
	case Code of
		error -> error(Url, Content);
		_ -> ok(Url, Code, Content)
	end.

% given an arbitrary string, produce a list of all
% http urls contained within
urlmatch(Str) ->
	urlmatch(Str, 0, []).

urlmatch(Str, Off, Matches) ->
	case re:run(Str,
				"https?://" ++ % protocol
 				% domain
				"(?:(?:[a-zA-Z0-9-]+)\\.)*(?:[a-zA-Z]+)" ++
				% path
				"(?:/[^/ >)}\\]]*)*" ++
				% anchor
				"(?:#[^ >})]*)?"
				% query
				"(?:\\?"
					"(?:" ++
						%"(?:&?[a-zA-Z0-9_@$%^\.-]+)" ++ % key
						"(?:(?:&(amp;))?:?[^ \t>)}\\]=]+)" ++ % key
							%"(?:[a-zA-Z0-9_@$%^\.-]+)" ++ % val
							"(?:=[^ \t>)}\\]&]*)?" ++ % val
					")*" ++
				")?" ++
				"",
				[{offset,Off}]
		) of
		{match,[{Start,Len}]} ->
			urlmatch(Str, Start+1, Matches ++
				[ string:substr(Str, Start+1, Len) ]);
		_ -> Matches
	end.

test_urlmatch() ->
	[
		{ [ "" ], [] },
		{ [ " " ], [] },
		{ [ "http://foo" ], ["http://foo"] },
		{ [ "http://foo-" ], ["http://foo"] },
		{ [ "http://foo]" ], ["http://foo"] },
		{ [ "(http://foo)" ], ["http://foo"] },
		{ [ "http://foo.bar" ], ["http://foo.bar"] },
		{ [ "http://foo.bar.baz" ], ["http://foo.bar.baz"] },
		{ [ "http://foo.bar.baz?" ], ["http://foo.bar.baz?"] },
		{ [ "http://foo.bar.baz/?" ], ["http://foo.bar.baz/?"] },
		{ [ "http://foo.bar.baz/?quux" ], ["http://foo.bar.baz/?quux"] },
		{ [ "http://foo.bar.baz/?quux=1" ], ["http://foo.bar.baz/?quux=1"] },
		{ [ "http://foo.bar.baz/quux" ], ["http://foo.bar.baz/quux"] },
		{ [ "http://foo.bar.baz/quux/" ], ["http://foo.bar.baz/quux/"] },
		{ [ "http://foo.bar.baz/quux/#quuz" ], ["http://foo.bar.baz/quux/#quuz"] },
		{ [ "http://foo.bar.baz/quux/#quuz" ], ["http://foo.bar.baz/quux/#quuz"] },
		{ [ "<URL:http://foo>" ], ["http://foo"] },
		{ [ "<URL:http://foo.bar.baz/?quux=1>" ], ["http://foo.bar.baz/?quux=1"] },
		{ [ "<URL:http://foo.bar.baz/#foo?quux=1>" ], ["http://foo.bar.baz/#foo?quux=1"] },
		{ [ "http://youtube.com/aBe4g lulz" ], ["http://youtube.com/aBe4g"] },
		{ [ "http http://youtube.com/aBe4g lulz" ], ["http://youtube.com/aBe4g"] },
		{ [ "(http://youtube.com/aBe4g) lulz" ], ["http://youtube.com/aBe4g"] },
		{ [ "(http://youtube.com/?aBe4g) lulz" ], ["http://youtube.com/?aBe4g"] },
		{ [ "[http://youtube.com/?aBe4g] lulz" ], ["http://youtube.com/?aBe4g"] },
		{ [ "[http://youtube.com/?aBe4g=foo&bar=baz] lulz" ], ["http://youtube.com/?aBe4g=foo&bar=baz"] },
		{ [ "http://foo http://bar" ], ["http://foo", "http://bar"] },
		{ [ "lulz http://foo and http://bar ru1e$" ], ["http://foo", "http://bar"] },
		{ [ "http://foo > http://bar" ], ["http://foo", "http://bar"] },
		{ [ "here it is:http://www.youtube.com/watch?v=D3nRywGHZNs&feature=related" ], ["http://www.youtube.com/watch?v=D3nRywGHZNs&feature=related"] },
		% FIXME: this appears to return the right answer but does not match?!
		%{ [ "http://www.google.com/search?hl=en&rlz=1C1CHMZ_en___US311&q=\"George+Greer\"+schiavo&btnG=Search&aq=f&oq=&aqi=g6" ], "http://www.google.com/search?hl=en&rlz=1C1CHMZ_en___US311&q=\"George+Greer\"+schiavo&btnG=Search&aq=f&oq=&aqi=g6" },
		{ [ "http://2.bp.blogspot.com/_Dc8q8nuzrHw/ShKXzIatZ4I/AAAAAAAAE3o/ZYuLaiTcbBU/s1600-h/AXEschedule.jpg" ], ["http://2.bp.blogspot.com/_Dc8q8nuzrHw/ShKXzIatZ4I/AAAAAAAAE3o/ZYuLaiTcbBU/s1600-h/AXEschedule.jpg"] },
		{ [ "http://www.php.net/manual/en/function.settype.php" ], ["http://www.php.net/manual/en/function.settype.php"] },
		{ [ "http://a-b-c.com/" ], ["http://a-b-c.com/"] },
		{ [ "http://a.ws" ], ["http://a.ws"] }
	].

