% ex: set ts=2 noet:
% given a string possibly containing URLs as input, parse out,
% visit, and report on each link; provide a Title and a tinyurl
% link if valid and an http error code if not

% NOTE: the mochiweb package i'm using crashes
% regularly on random web pages.

-module(urlinfo).
-export([test/0, loop/0, info/1, urlmatch/1, isParsable/1]).

-include_lib("../irc.hrl").
-import(irc).
-import(ircutil).
-import(test).
-define(timeout, 5000).

loop() ->
	receive
		{act, Pid, _Irc, #ircmsg{rawtxt=Rawtxt}=Msg, Dst, Nick, ["url", Url ]} ->
			act(Pid, Msg, Rawtxt, Dst, Nick, Url),
			loop();
		{act, Pid, _Irc, #ircmsg{rawtxt=Rawtxt}, Dst, _Nick, _Txt} ->
			scan_for_urls(Pid, Rawtxt, Dst),
			loop();
		{act, _, _, _, _, _, _} ->
			loop();
		{help, Pid, Dst, Nick} ->
			Pid ! {q,
				irc:resp(Dst, Nick, Nick ++ ": " ++
					"[\"url\", Url] -> fetch a URL's title and tinyurl")
			},
			loop()
	end.

% 
act(Pid, Msg, Rawtxt, Dst, Nick, Url) ->
	Output = info(Url, Rawtxt),
	if
		Output /= nil ->
			Pid ! { pipe, Msg, irc:resp(Dst, Nick, Output) };
		true -> nil
	end.

info(Url) ->
	info(Url, "").

info(Url, Rawtxt) ->
	{Code, Content} = content(Url),
	io:format("info Url=~s Code=~p Content=...~n",
		[Url,Code]),
	case Code of
		notparsable -> nil;
		error -> error(Url, Content);
		_ -> ok(Url, Code, Content, Rawtxt)
	end.

% given a line of IRC input, scan it for URLs and
% investigate each, producing a one-line report
scan_for_urls(Pid, Rawtxt, Dst) ->
	Urls = % detect output from other abbots
		case re:run(Rawtxt,
			"(" ++
				% now we always prefix to detect other bots
				"^url(?:info)? " ++
				"|" ++
				"/\\." % avoid mod_spox's slashdot(!)
			")") of
			{match,_} -> [];	% from another abbot, ignore URLs!
												% this avoids an endless feedback cycle
			_ -> urlinfo:urlmatch(Rawtxt)
			end,
	[ spawn(
		fun() ->
			Output = info(Url, Rawtxt),
			if
				Output == nil -> nil;
				true -> Pid ! {q, irc:privmsg(Dst, Output)}
			end
		end) || Url <- Urls
	].

test() ->
	test:unit(urlinfo,
		[
			{ urlmatch, test_urlmatch() }
		]).

% see if the Content-Type header indicates an html or xml type document
% NOTE: http:request downcases all http headers
isParsable([]) -> true;
isParsable([{Key,Val}|Rest]) ->
	if
		"content-type" == Key ->
			(
				 ("text/plain" == string:substr(Val, 1, 10))
			or ("text/html" == string:substr(Val, 1, 9))
			or ("text/xhtml" == string:substr(Val, 1, 10))
			or ("text/xhtml+xml" == string:substr(Val, 1, 14))
			or ("application/xhtml+xml" == string:substr(Val, 1, 21))
			);
		true -> isParsable(Rest)
	end.

% return HTML content
% non-HTML or XHTML is considered an error
content(Url) ->
	case http:request(get,{Url,[]},[{timeout,?timeout}],[]) of
		{ok, {{_Http, Code, _Msg}, Headers, Content}} ->
			case isParsable(Headers) of
				true -> {Code, Content};
				false -> {notparsable, notHtml}
			end;
		{error, Why} -> {error, Why}
		end.

title(Content) ->
	T = title_(Content),
	T2 =
		if
			is_binary(T) -> binary_to_list(T);
			true -> T
		end,
	normalize_title(T2).

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
			util:hd(T, "")
	end.

% remove any weird chars from title, especially \r and \n.
% trim title afterwards.
normalize_title(Title) ->
	Nice =
		lists:filter(
			fun(C) -> % sanitize title contents
				char:isalnum(C) or char:ispunct(C) or (C == 32)
			end, Title),
	util:trim(Nice, 32).

tinyurl(Url) ->
	Url2 = "http://tinyurl.com/api-create.php?url=" ++ Url,
	{_Code, Content} = content(Url2),
	Content.

% url fetc failed, display url and error
error(Url, Code) ->
	lists:flatten(
		io_lib:format("url ~s -> ~s", [Url, Code])).

% url fetch succeeded, display url, title and tinyurl
ok(Url, _Code, Content, Rawtxt) ->
	Title = title(Content),
	io:format("urlinfo ok Title=~p Rawtxt=~p~n",
		[Title, Rawtxt]),
	case (Title == "") or (string:str(Rawtxt, Title) /= 0) of
		true ->
			% the string we parsed the url from already contains
			% the entire URL's title; that means it is probably from
			% another bot's search result. don't send anything.
			nil;
		false ->
			TinyURL = 
				if 
					length(Url) < 30 -> ""; % short already
					true -> tinyurl(Url)
				end,
			Title2 = ircutil:dotdotdot(cgi:entity_decode(Title), 70),
			lists:flatten(
				io_lib:format("url \"~s\" ~s",
					[Title2, TinyURL]))
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

