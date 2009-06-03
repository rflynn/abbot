% ex: set ts=2 noet:
%  

-module(translate).
-author("pizza@parseerror.com").
-export(
	[
		test/0,
		loop/0
	]).

-include_lib("../irc.hrl").
-import(test).
-import(irc).

-define(timeout, 5000).
-define(babelfish_url, "http://babelfish.yahoo.com/translate_txt").

test() ->
	true.

loop() ->
	receive
		{ act, Pid, _Irc, _, Dst, Nick, ["translate", FromLang, ToLang | Txt]} ->
			xl(Pid, Dst, Nick, FromLang, ToLang, Txt),
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
          	"[\"translate\", \"attach\", Who, From, To] -> Automatically Translate To/From for Who",
            "Langs = [" ++ util:join(",", [ Short || {Short,_} <- langs() ]) ++ "]"
					]
				]},
			loop()
	end.

xl(Pid, Dst, Nick, From, To, Txt_) ->
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
		_ -> ok(Pid, Dst, Nick, Content)
	end.

content(From, To, Txt) ->
	Fields =
		[
		 { "ei",			"ISO-8859-1"			},
  	 { "doit",	 	"done"    				},
  	 { "fr",			"bf-res"  				},
  	 { "intl",		"1"       				},
  	 { "tt",			"urltext" 				},
			% FIXME: lack of encoding user-supplied data considered harmful
  	 { "lp",			From ++ "_" ++ To	},	
  	 { "trtext",	Txt       				},
  	 { "inp_btn",	"Translate"				}],
	io:format("Header Fields=~p~n", [Fields]),
	case http:request(post,
		{
			?babelfish_url,																				% URL
			[{ "Referer", ?babelfish_url }, 											% Headers
			 { "User-Agent", "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0)" }],
			"application/x-www-form-urlencoded",									% Encoding
			util:join("&", [ K ++ "=" ++ V || {K,V} <- Fields ])	% POST fields
		},
		[{timeout, ?timeout}],
		[{body_format, binary}]
	) of
		{ok, {{_Http, Code, _Msg}, _Header, Content}} -> {Code, Content};
		{error, Why} -> {error, Why}
		end.

ok(Pid, Dst, Nick, Contents) ->
	Result = result(Contents),	% FIXME: normalize result
															% NOTE: result may be in crazy different char set
	Pid ! {q, irc:resp(Dst, Nick, Result) }.

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
