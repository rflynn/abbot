% ex: set ts=2 noet:
%  

-module(unix).
-author("pizza@parseerror.com").
-export(
	[
		test/0,
		loop/0
	]).

-include_lib("../irc.hrl").
-import(test).
-import(irc).
-import(cgi).
-import(util).

-define(md5r_timeout, 5000).
%-define(md5r_url, "http://md5.gromweb.com/query/").
-define(md5r_url, "http://md5.noisette.ch/md5.php?hash=").

test() ->
	true.

loop() ->
	receive
		{ act, Pid, Irc, Msg, Dst, Nick, ["dig" , Domain]} ->
			dig(Pid, Irc, Msg, Dst, Nick, Domain, "a"),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["dig", Type, Domain]} ->
			dig(Pid, Irc, Msg, Dst, Nick, Domain, Type),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["echo" | Txt]} ->
			echo(Pid, Irc, Msg, Dst, Nick, Txt),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["md5" | Txt]} ->
			md5(Pid, Irc, Msg, Dst, Nick, Txt),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["md5r", MD5Sum]} ->
			md5r(Pid, Irc, Msg, Dst, Nick, MD5Sum),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["sort" | Txt]} ->
			sort(Pid, Irc, Msg, Dst, Nick, Txt),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["wc" | Txt]} ->
			wc(Pid, Irc, Msg, Dst, Nick, Txt),
			loop();
		{ act, _, _, _, _, _, _ } ->
			loop();
		{ help, Pid, Dst, Nick } ->
			Pid ! { q,
				[ irc:resp(Dst, Nick, Nick ++ ": " ++ Txt) || Txt <-
					[
						"[\"dig\",  | Domain] -> DNS records check",
						"[\"echo\", | Txt] -> print Txt",
						"[\"md5\",  | Txt] -> md5 checksum Txt",
						"[\"md5r\", | MD5Sum] -> reverse an md5 sum",
						"[\"sort\"  | Txt] -> sort Txt",
						"[\"wc\"    | Txt] -> word count"
					]
				] },
			loop()
	end.

dig(_, _, _, _, _, [], _) -> nil;
dig(Pid, _, Msg, Dst, Nick, Domain, "a")   -> dig_(Pid, Msg, Dst, Nick, Domain, "a");
dig(Pid, _, Msg, Dst, Nick, Domain, "any") -> dig_(Pid, Msg, Dst, Nick, Domain, "any");
dig(Pid, _, Msg, Dst, Nick, Domain, "mx")  -> dig_(Pid, Msg, Dst, Nick, Domain, "mx");
dig(Pid, _, Msg, Dst, Nick, Domain, "txt") -> dig_(Pid, Msg, Dst, Nick, Domain, "txt");
dig(_, _, _, _, _, _, _) -> nil.

dig_(Pid, Msg, Dst, Nick, Domain, Type) ->
	{ok, Re} = re:compile("^([a-zA-Z0-9-]+\.)+[a-zA-Z]+$"),
	case re:run(Domain, Re) of
		nomatch -> nil;
		{match, _} ->
			Run = "dig " ++ Type ++ " " ++ Domain ++ " | grep \"^[^;]\" | head -n 1 | sed -e's/\\t/ /g'",
  		Out = os:cmd(Run),
			Lines = string:tokens(Out, "\r\n"),
			Res = hd(Lines),
			Pid ! {pipe, Msg, irc:resp(Dst, Nick, Res)}
		end.

echo(_, _, _, _, _, []) -> nil;
echo(Pid, _, Msg, Dst, Nick, Txt) ->
	Res = util:j(Txt),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Res)}.

grep(_, _, _, _, _, _, []) -> nil;
grep(Pid, _, Msg, Dst, Nick, Regexp, Txt) ->
	Txt2 = util:j(Txt),
	case regexp:match(Txt2, Regexp) of
		{match,_,_} -> Pid ! {pipe, Msg, irc:resp(Dst, Nick, Txt2)};
		true        -> nil
	end.

md5(_, _, _, _, _, []) -> nil;
md5(Pid, _, Msg, Dst, Nick, _) ->
	In = string:substr(Msg#ircmsg.rawtxt, length("md5")+1),
	In2 = util:ltrim(In),
	L = binary_to_list(erlang:md5(In2)),
	Hex = [ util:hex(X, 2) || X <- L ],
	Res = util:join("", Hex),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Res)}.

md5r(Pid, _Irc, Msg, Dst, Nick, MD5Sum) ->
	case length(MD5Sum) of
		32 ->
  		Url = ?md5r_url ++ cgi:url_encode(MD5Sum),
  		case http:request(get,{Url,[]},[{timeout, ?md5r_timeout}],[]) of
    		{ok, {{_Http, _Code, _Msg}, _Header, Content}} ->
					Content2 =
						case string:str(Content, "</error>") of
							0 ->
								% fugly
								Prefix = string:substr(Content, string:rstr(Content, "<![CDATA[")+9),
								"\"" ++ string:substr(Prefix, 1, string:rstr(Prefix, "]]")-1) ++ "\"";
							_ -> "?"
							end,
					Resp = Nick ++ ": " ++ MD5Sum ++ " = md5(" ++ Content2 ++ ")",
					Pid ! {pipe, Msg, irc:resp(Dst, Nick, Resp)};
    		{error, _Why} -> nil
    		end;
		_ -> nil
	end.

sort(Pid, _, Msg, Dst, Nick, ["-r" | Txt]) ->
	Res = util:j(lists:reverse(lists:sort(Txt))),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Res)};
sort(Pid, _, Msg, Dst, Nick, Txt) ->
	Res = util:j(lists:sort(Txt)),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Res)}.

wc(Pid, _, Msg, Dst, Nick, Txt) ->
	Cnt = length(Txt),
	Res = integer_to_list(Cnt),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Res)}.

