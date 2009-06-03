% ex: set ts=2 noet:
%  

-module(translate).
-author("pizza@parseerror.com").
-export(
	[
		test/0,
		loop/0,
		charswap/4,
		en_aol/1,
		aol_en/1
	]).

-include_lib("../irc.hrl").
-import(test).
-import(irc).
-import(cgi).

-define(timeout, 5000).
-define(babelfish_url, "http://babelfish.yahoo.com/translate_txt").

test() ->
	true.

loop() ->
	receive
		{ act, Pid, _Irc, Msg, Dst, Nick, ["translate", FromLang, ToLang | Txt]} ->
			xl(Pid, Msg, Dst, Nick, FromLang, ToLang, Txt),
			loop();
		%{ act, Pid, Irc, _, Dst, Nick, ["translate", "attach", Who, FromLang, ToLang]} ->
		%	attach(Pid, Irc, Dst, Nick, Who, FromLang, ToLang),
		%	loop();
		{ act, _, _, _, _, _, _ } ->
			loop();
		{ help, Pid, Dst, Nick } ->
			Pid ! {q,
        [ irc:resp(Dst, Nick, Nick ++ ": " ++ Que) || Que <-
          [ "[\"translate\", From, To | Txt ] -> Translate Txt From language To language",
          	%"[\"translate\", \"attach\", Who, From, To] -> Automatically Translate To/From for Who",
            "Langs = [" ++ util:join(",", [ Short || {Short,_} <- langs() ]) ++ "]"
					]
				]},
			loop()
	end.

xl(Pid, Msg, Dst, Nick, "en", "engrish", Txt_) ->
	Txt = util:j(Txt_),
	Result = charswap(Txt, [], $l, $r),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Result) };

xl(Pid, Msg, Dst, Nick, "en", "aol", Txt_) ->
	Txt = util:j(Txt_),
	Result = en_aol(Txt),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Result) };

xl(Pid, Msg, Dst, Nick, "aol", "en", Txt_) ->
	Txt = util:j(Txt_),
	Result = aol_en(Txt),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Result) };

xl(Pid, Msg, Dst, Nick, From, To, Txt_) ->
	Txt = util:j(Txt_),
	io:format("translate From=~p To=~p Txt_=~p Txt=~p~n",
		[From, To, Txt_, Txt]),
	{Code, Content} = content(From, To, Txt),
	io:format("translate Code=~p~n",[Code]),
	case Code of
		error ->
			Pid ! {q, irc:resp(Dst, Nick,
				lists:flatten(
					io_lib:format("translate -> ~s", [Code])))};
		_ -> ok(Pid, Msg, Dst, Nick, Content)
	end.

content(From, To, Txt) ->
	Post =
		[
		 { "ei",			"ISO-8859-1"			},
  	 { "doit",	 	"done"    				},
  	 { "fr",			"bf-res"  				},
  	 { "intl",		"1"       				},
  	 { "tt",			"urltext" 				},
  	 { "lp",			From ++ "_" ++ To	},	
  	 { "trtext",	Txt       				},
  	 { "inp_btn",	"Translate"				}],
	io:format("Header Post=~p~n", [Post]),
	case http:request(post,
		{
			?babelfish_url,																				% URL
			[{ "Referer", ?babelfish_url }, 											% Headers
			 { "User-Agent", "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0)" }],
			"application/x-www-form-urlencoded",									% Encoding
				util:join("&",
					[ K ++ "=" ++ cgi:url_encode(V) || {K,V} <- Post ])	% POST fields
		},
		[{timeout, ?timeout}],
		[{body_format, binary}]
	) of
		{ok, {{_Http, Code, _Msg}, _Header, Content}} -> {Code, Content};
		{error, Why} -> {error, Why}
		end.

ok(Pid, Msg, Dst, Nick, Contents) ->
	Result = result(Contents),	% FIXME: normalize result
															% NOTE: result may be in crazy different char set
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Result) }.

result([]) -> [];
result(Contents) ->
	% <form action="http://search.yahoo.com/search;_ylt=A0LEUFiT9CVKkZEAsDy37s4F" method=get>
	%    <div id="result"><div style="padding:0.6em;">foo</div></div>
	Doc = mochiweb_html:parse(Contents),
	Mapping = [{<<"link">>,1},{<<"myUrl">>,2}],
	F = fun(_Ctx, [String]) -> % mysterious
		proplists:get_value(String, Mapping, 0) end,
	MyFuns = [{my_fun, F, [string]}],
	Result = mochiweb_xpath:execute("//div[@id='result']/div/text()", Doc, MyFuns),
	io:format("translate Result=~p~n", [Result]),
	Res = if
		(length(Result) > 0) ->
			R = hd(Result),
			if is_binary(R) -> binary_to_list(R);
				true -> R
			end;
		true -> []
	end,
	io:format("translate Res=~p~n", [Res]),
	Res.

%attach(Pid, Irc, Dst, Nick, Who, FromLang, ToLang) ->
%	nil.

langs() ->
	[
		{ "de", "German"				},
		{ "el", "Greek"					},
		{ "es", "Spanish"				},
		{ "fr", "French"				},
		{ "it", "Italian"				},
		{ "ja", "Japanese"			},
		{ "ko", "Korean"				},
		{ "nl", "Dutch"					},
		{ "pt", "Portugese"			},
		{ "ru", "Russian"				},
		{ "zh", "Chinese-simp"	},
		{ "zt", "Chinese"				}
	].

% utility functions for silly easter eggs

charswap([], Wr, _, _) -> Wr;
charswap([A|Rd], Wr, A, B) ->
	charswap(Rd, Wr ++ [B], A, B);
charswap([B|Rd], Wr, A, B) ->
	charswap(Rd, Wr ++ [A], A, B);
charswap("City" ++ Rd, Wr, A, B) ->
	charswap(Rd, Wr ++ "Shitty", A, B);
charswap("city" ++ Rd, Wr, A, B) ->
	charswap(Rd, Wr ++ "shitty", A, B);
charswap([X|Rd], Wr, A, B) ->
	charswap(Rd, Wr ++ [X], A, B).

en_aol(Str) -> en_aol(string:to_upper(Str), []).
en_aol([],      Wr) -> Wr ++ "!!!1";
en_aol([$A|Rd], Wr) -> en_aol(Rd, Wr ++ [$@]);
en_aol([$B|Rd], Wr) -> en_aol(Rd, Wr ++ [$|, $3]);
en_aol([$I|Rd], Wr) -> en_aol(Rd, Wr ++ [$1]);
en_aol([$K|Rd], Wr) -> en_aol(Rd, Wr ++ [$|, $<]);
en_aol([$O|Rd], Wr) -> en_aol(Rd, Wr ++ [$0]);
en_aol([$S|Rd], Wr) -> en_aol(Rd, Wr ++ [$$]);
en_aol("the" ++ Rd, Wr)	-> en_aol(Rd, Wr ++ "D@");
en_aol("too" ++ Rd, Wr)	-> en_aol(Rd, Wr ++ "2");
en_aol("to"  ++ Rd, Wr)	-> en_aol(Rd, Wr ++ "2");
en_aol("you"  ++ Rd, Wr)	-> en_aol(Rd, Wr ++ "U");
en_aol("haha" ++ Rd, Wr)	-> en_aol(Rd, Wr ++ "LULZ!");
en_aol("very" ++ Rd, Wr) -> en_aol(Rd, Wr ++ "WAI");
en_aol("way"  ++ Rd, Wr)	-> en_aol(Rd, Wr ++ "WAI");
en_aol([R|Rd], []) -> en_aol(Rd, [R]);
en_aol([R|Rd], Wr) -> en_aol(Rd, Wr ++ [R]).

aol_en(Str) -> aol_en(string:to_lower(Str), []).
aol_en([],      Wr) -> Wr;
aol_en([$!|Rd], Wr) -> aol_en(Rd, Wr);
aol_en([$1|Rd], Wr) -> aol_en(Rd, Wr);
aol_en("???" ++ Rd, Wr)	-> aol_en(Rd, Wr ++ "?");
aol_en([$?|Rd], Wr) 		-> aol_en(Rd, Wr);
aol_en(" da " ++ Rd, Wr)-> aol_en(Rd, Wr ++ " the ");
aol_en("omg" ++ Rd, Wr)-> aol_en(Rd, Wr ++ "oh my");
aol_en("helo" ++ Rd, Wr)-> aol_en(Rd, Wr ++ "hello");
aol_en("lulz" ++ Rd, Wr)-> aol_en(Rd, Wr ++ "haha");
aol_en("wtf" ++ Rd, Wr)	-> aol_en(Rd, Wr);
aol_en("lol" ++ Rd, Wr)	-> aol_en(Rd, Wr ++ "haha");
aol_en("ur " ++ Rd, Wr)	-> aol_en(Rd, Wr ++ "your ");
aol_en(" u " ++ Rd, Wr)	-> aol_en(Rd, Wr ++ " you ");
aol_en(" u!" ++ Rd, Wr)	-> aol_en(Rd, Wr ++ " you");
aol_en("u?" ++ Rd, Wr)	-> aol_en(Rd, Wr ++ "you?");
aol_en("r " ++ Rd, Wr)	-> aol_en(Rd, Wr ++ "are ");
aol_en(" wai" ++ Rd, Wr)	-> aol_en(Rd, Wr ++ " way");
aol_en("i m " ++ Rd, Wr)	-> aol_en(Rd, Wr ++ "i am ");
aol_en("awsum" ++ Rd, Wr)	-> aol_en(Rd, Wr ++ "awesome");
aol_en("fien" ++ Rd, Wr)	-> aol_en(Rd, Wr ++ "fine");
aol_en([R|Rd], []) -> aol_en(Rd, [R]);
aol_en([R|Rd], Wr) -> aol_en(Rd, Wr ++ [R]).

% "HELO HOW R U??!?! WTF IM FIEN!11!1!11 WTF"
% "OMG I M SO FCKIG AWSUM!!!!!1 CUM WURSHP MEE BCH!!!!!1" 

