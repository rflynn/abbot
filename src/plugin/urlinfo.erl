% ex: set ts=2 noet:
% given a string possibly containing URLs as input, parse out,
% visit, and report on each link; provide a Title and a tinyurl
% link if valid and an http error code if not

% NOTE: the mochiweb package i'm using crashes
% regularly on random web pages.


-module(urlinfo).
-export([test/0, loop/0, info/1, urlmatch/1, do_urlscan/5, isParsable/1, substr_count/2, url_title_fuzzymatch/2]).
-author("parseerror@gmail.com").

-include_lib("../irc.hrl").
-import(irc).
-import(ircutil).
-import(test).
-define(timeout, 30000).
-define(min_url_len_tinyurl, 50). % URLs shorter than this are not translated

% FIXME: hard-coded nicks that generate copious URLs
-define(nick_ignore,
	[
		"mod_spox"
	]).

% hard-coded list of url prefixes to ignore; reduces junk
% output for urls mentioned commonly and/or by other bots
-define(scan_ignore,
	[
		"dev.modspox.com", % mod_spox's home
		"woot.com", % mod_spox mentions this one often for some reason
		"php.net/manual" % mod_spox's php function lookup
	]).

loop() ->
	receive
		{act, Pid, _Irc, #ircmsg{rawtxt=Rawtxt}=Msg, Dst, Nick, ["url", "txt", Url ]} ->
			urltxt(Pid, Msg, Rawtxt, Dst, Nick, Url),
			loop();
		{act, Pid, _Irc, #ircmsg{rawtxt=Rawtxt}=Msg, Dst, Nick, ["url", Url ]} ->
			act(Pid, Msg, Rawtxt, Dst, Nick, Url),
			loop();
		{act, Pid, Irc, #ircmsg{rawtxt=Rawtxt}, Dst, Nick, _Txt} ->
			io:format("urlinfo Nick=~s mynick=~s~n", [Nick,(Irc#ircconn.user)#ircsrc.nick]),
			% FIXME: need a way to detect pipelined messages!
			% FIXME: pipelined message Nick is the original nick who sent it, not me.
			case lists:member(Nick, ?nick_ignore) or (Nick == (Irc#ircconn.user)#ircsrc.nick) of
				true -> nil; % ignore
				_ -> urlscan(Irc, Pid, Rawtxt, Dst, ?scan_ignore)
			end,
			loop();
		{act, _, _, _, _, _, _} ->
			loop();
		{help, Pid, Dst, Nick} ->
			Pid ! {q,
				[	irc:resp(Dst, Nick, Nick ++ ": " ++ Txt) || Txt <-
					[ "[\"url\", Url] -> fetch URL and report",
					  "[\"url\", \"txt\", Url] -> display page text" ]
				]
			},
			loop()
	end.

urltxt(Pid, Msg, _Rawtxt, Dst, Nick, Url) ->
	Raw = txt(Url),
	Txt = ircutil:dotdotdot(Raw, 400),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Txt)}.

txt(Url) ->
	{Code, Content} = content(Url),
	io:format("info Url=~s Code=~p Content=...~n",
		[Url,Code]),
	case Code of
		notparsable -> "";
		error -> "";
		_ -> txt_(Content)
	end.

txt_(Content) ->
% {{badmatch,[]},[{mochiweb_html,parse_tokens,1},{urlinfo,txt_,1},{urlinfo,urltxt,6},{urlinfo,loop,0}]}
	Doc = 
		case mochiweb_html:parse(Content) of
			{badmatch,[]} -> "";
			Match -> Match
		end,
	Mapping = [{<<"link">>,1},{<<"myUrl">>,2}],
	F =
		fun(_Ctx, [String]) ->
			proplists:get_value(String, Mapping, 0) end,
	MyFuns = [{my_fun, F, [string]}],
	% FIXME: would be much cleaner if we could include or exclude based
	% on the name of the tag, but this xpath implementation seems to
	% not support that(!)
	Bin = mochiweb_xpath:execute("//body//text()", Doc, MyFuns),
	io:format("Bin=~p~n", [Bin]),
	Txt = [ binary_to_list(B) || B <- lists:flatten(Bin) ],
	NoScript = lists:filter(
		fun(Str) -> % hack to disregard much CSS and javascript
			(string:str(Str, "#")				== 0) and
			(string:str(Str, "{")				== 0) and
			(string:str(Str, "}")				== 0) and
			(string:str(Str, "var ")		== 0) and
			(string:str(Str, "if (")		== 0) and
			(string:str(Str, "false;")	== 0)
		end, Txt),
	Txt2 = util:j(ircutil:stripjunk(NoScript)),
	Txt3 = util:j(string:tokens(Txt2, "\r\n\t")),
	Txt3.


% 
act(Pid, Msg, Rawtxt, Dst, Nick, Url) ->
	Output = info(Url, Rawtxt, false),
	if
		Output /= nil ->
			Pid ! { pipe, Msg, irc:resp(Dst, Nick, Output) };
		true -> nil
	end.

info(Url) ->
	info(Url, "", false).

info(Url, Rawtxt, Scanned) ->
	{Code, Content} = content(Url),
	io:format("info Url=~s Code=~p Content=...~n",
		[Url,Code]),
	case Code of
		notparsable -> nil;
		error ->
			if
				enetunreach == Content -> nil; % get this sometimes, not sure why
				true -> error(Url, Content)
			end;
		_ -> ok(Url, Code, Content, Rawtxt, Scanned)
	end.

% given a line of IRC input, scan it for URLs and
% investigate each, producing a one-line report
urlscan(Irc, Pid, Rawtxt, Dst, Ignore) ->
	[ spawn(
		fun() ->
			Output = info(Url, Rawtxt, true),
			if
				Output == nil -> nil;
				true -> Pid ! {q, irc:privmsg(Dst, Output)}
			end
		end) || Url <- do_urlscan(Irc, Pid, Dst, Rawtxt, Ignore)
	].

% given a line of IRC input, scan it for URLs
do_urlscan(Irc, Pid, Dst, Rawtxt, Ignore) ->
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
	Urls2 = % filter ignored
		lists:filter(
			fun(Url) ->
 				lists:all(
					fun(Ign) -> string:str(Url, Ign) == 0 end, Ignore)
				end,
		Urls),
	% check that the URL is not the most recent sent to this Dst
	LastURL = irc:state(Irc, {urlinfo,Dst}, ""),
	Urls3 = lists:filter(fun(U) -> U /= LastURL end, Urls2),
	if
		length(Urls3) > 0 ->
			NewLastURL = lists:last(Urls3),
			Pid ! {setstate, {urlinfo,Dst}, NewLastURL};
		true -> nil
	end,
	Urls3.

test() ->
	test:unit(urlinfo,
		[
			{ urlmatch,             test_urlmatch()             },
			%{ do_urlscan,           test_dourlscan()            },
			{ substr_count,         test_substr_count()         },
			{ url_title_fuzzymatch, test_url_title_fuzzymatch() }
		]).

% see if the Content-Type header indicates an html or xml type document
% NOTE: http:request downcases all http headers
isParsable([]) -> true;
isParsable([{Key,Val}|Rest]) ->
	if
		"content-type" == Key ->
			(
				 (string:substr(Val, 1, 10) == "text/plain")
			or (string:substr(Val, 1,  9) == "text/html")
			or (string:substr(Val, 1, 10) == "text/xhtml")
			or (string:substr(Val, 1, 14) == "text/xhtml+xml")
			or (string:substr(Val, 1, 21) == "application/xhtml+xml")
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
% FIXME: mochiweb html parser crashes frequently; i spawn() URLs separately so the bot doesn't crash,
% but crashing does kill any URL feedback. launch the parser in yet another separate process so that we can still report something
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
ok(Url, _Code, Content, Rawtxt, Scanned) ->
	Title = title(Content),
	io:format("urlinfo ok Title=~p Rawtxt=~p~n",
		[Title, Rawtxt]),
	% only filter based on quality if this was a passively-scannd URL; if we were specifically asked to
	% look it up, always response.
	% TODO: this is not the right place for this, refactor
	case Scanned and ((Title == "") or (string:str(Rawtxt, Title) /= 0) or (url_title_fuzzymatch(Url, Title))) of
		true ->
			% the string we parsed the url from already contains
			% the entire URL's title; that means it is probably from
			% another bot's search result. don't send anything.
		  % ...or the title is (mostly) contained in the URL already,
		  % don't bother reprinting it.
			nil;
		false ->
			Title2 = ircutil:dotdotdot(cgi:entity_decode(Title), 150),
			lists:flatten(
				io_lib:format("url \"~s\"", [Title2]))
	end.

% does Str exist as a substring in at least one entry of a list of strings?
is_substr_list(Substr, List) ->
	lists:any(fun(B) -> B end,
		lists:map(fun(S)->string:str(S, Substr) /= 0 end, List)).

% count true values in list
count(List) -> count(List, 0).
count([],       Cnt) -> Cnt;
count([true|T], Cnt) -> count(T, Cnt+1);
count([_|T],    Cnt) -> count(T, Cnt).

substr_count(SubstrList, StrList) ->
	count(lists:map(fun(Substr) -> is_substr_list(Substr, StrList) end, SubstrList)).

test_substr_count() ->
	[
		{ [ [   ],     [       ] ], 0 },
		{ [ ["" ],     [""     ] ], 0 },
		{ [ ["a"],     [       ] ], 0 },
		{ [ [   ],     ["a"    ] ], 0 },
		{ [ ["a"],     ["b"    ] ], 0 },
		{ [ ["b"],     ["a"    ] ], 0 },
		{ [ ["a"],     ["a"    ] ], 1 },
		{ [ ["a","b"], ["a"    ] ], 1 },
		{ [ ["a"],     ["a","a"] ], 1 },
		{ [ ["a"],     ["aa"   ] ], 1 },
		{ [ ["b"],     ["ba"   ] ], 1 },
		{ [ ["b"],     ["ab"   ] ], 1 },
		{ [ ["b"],     ["bb"   ] ], 1 },
		{ [ ["a","b"], ["a","b"] ], 2 },
		{ [ ["a","b"], ["b","a"] ], 2 }
	].

% many sites now have descriptive URLs (but some don't)
% let's try and decide if the URL contains most of the title words
% tokenize url and page title and figure out how similar they are
% better to err on the side of false positives
url_title_fuzzymatch(Url, Title) ->
	SplitBy = " :/.-?!&|_'", % no standard for [[[:punct:]], eh?
	UrlTok = string:tokens(string:to_lower(Url), SplitBy),
	TitleTok = string:tokens(string:to_lower(Title), SplitBy),
	case length(TitleTok) of
		0 -> true;
		N -> substr_count(TitleTok, UrlTok) / N >= 2/3
		end.

test_url_title_fuzzymatch() ->
	[
		{ [ "", "" ], true },
		{ [ " ", " " ], true },
		{ [ "http://www.yahoo.com/", "Yahoo!" ], true },
		{ [ "http://www.jimspancakes.com/2010/06/just-got-an-iphone-4g-its-delicious/", "Just got an iPhone 4g- it's delicious! - Pancake art | Jim's Pancakes" ], true }
	].

% given an arbitrary string, produce a list of all
% http urls contained within
urlmatch(Str) ->
	urlmatch(Str, 0, []).

urlmatch(Str, Off, Matches) ->
	case re:run(Str,
				"https?://" ++ % protocol
 				% domain
				"(?:"
					"(?:(?:[a-zA-Z0-9-]+)\\.)*(?:[a-zA-Z]+)" ++
					"|(?:[0-9]{1,3}\.){3}[0-9]{1,3}"
				")"
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
		{ [ "http://a.ws" ], ["http://a.ws"] },
		{ [ "http://208.68.18.97" ], ["http://208.68.18.97"] },
		{ [ "http://pastebin.ca/1" ], ["http://pastebin.ca/1"] }
	].

test_dourlscan() ->
	[
		{ [ "", [] ], [] },
		{ [ " ", [] ], [] },
		{ [ "http://foo", [] ], ["http://foo"] },
		{ [ "http://a http://b", [] ], ["http://a", "http://b"] },
		{ [ "http://pastebin.ca/1", [] ], ["http://pastebin.ca/1"] },
		% test ignore list
		{ [ "http://a.com", ["b.com"] ], ["http://a.com"] },
		{ [ "http://a.com", ["a.com"] ], [] },
		{ [ "http://a.com http://b.com", ["a.com"] ], ["http://b.com"] },
		{ [ "http://a.com http://b.com", ["a.com","b.com"] ], [] }
	].

