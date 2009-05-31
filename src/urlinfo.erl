% ex: set ts=2 noet:
% given a string possibly containing URLs as input, parse out,
% visit, and report on each link; provide a title and a tinyurl
% link if valid and an http error code if not

% NOTE: the mochiweb package i'm using crashes
% regularly on random web pages.

-module(urlinfo).
-export([test/0, info/1, urlmatch/1]).
-import(test).

test() ->
	test:unit(urlinfo,
		[
			{ urlmatch, test_urlmatch() }
		]).

content(Url) ->
	case http:request(get,{Url,[]},[{timeout,2000}],[]) of
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
	R = mochiweb_xpath:execute("/HTML/HEAD/TITLE/text()", Doc, MyFuns),
	if
		[] /= R -> hd(R);
		true ->
			hd(mochiweb_xpath:execute(
				"/html/head/title/text()", Doc, MyFuns))
	end.

tinyurl(Url) ->
	Url2 = "http://tinyurl.com/api-create.php?url=" ++ Url,
	{_Code, Content} = content(Url2),
	Content.

% url fetc failed, display url and error
error(Url, Code) ->
	lists:flatten(
		io_lib:format("~s -> ~s~n", [Url, Code])).

% url fetch succeeded, display url, title and tinyurl
ok(Url, _Code, Content) ->
	Title = title(Content),
	TitleTrim =
		if
			length(Title) > 30 ->
				string:substr(Title, 1, 27) ++"...";
			true -> Title
		end,
	TinyURL = tinyurl(Url),
	lists:flatten(
		io_lib:format("~s -> \"~s\" (~s)~n",
			[Url, TitleTrim, TinyURL])).

info(Url) ->
	{Code, Content} = content(Url),
	io:format("info Url=~s Code=~p Content=~p~n",
		[Url,Code,Content]),
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
				"(?:(?:[a-zA-Z0-9]+)\\.)*(?:[a-zA-Z]+)" ++
				% path
				"(?:/[a-zA-Z0-9-]*)*" ++
				% anchor
				"(?:#[^ >})]*)?"
				% query
				"(?:[?]?"
					"(?:" ++
						"(?:[a-zA-Z0-9@$%^\.]+)" ++ % key
						"(?:" ++
							"=" ++
							"(?:[a-zA-Z0-9@$%^\.]+)[&]?" ++ % val
						")?" ++
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
		{ [ "here it is:http://www.youtube.com/watch?v=D3nRywGHZNs&feature=related" ], ["http://www.youtube.com/watch?v=D3nRywGHZNs&feature=related"] }
	].

