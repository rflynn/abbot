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
-import(util).

test() ->
	true.

loop() ->
	receive
		{ act, Pid, Irc, Msg, Dst, Nick, ["echo" | Txt]} ->
			echo(Pid, Irc, Msg, Dst, Nick, Txt),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["head" | Txt]} ->
			head(Pid, Irc, Msg, Dst, Nick, Txt),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["last" | Txt]} ->
			last(Pid, Irc, Msg, Dst, Nick, Txt),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["md5" | Txt]} ->
			md5(Pid, Irc, Msg, Dst, Nick, Txt),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["rep", N | Txt]} ->
			rep(Pid, Irc, Msg, Dst, Nick, list_to_integer(N), Txt),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["sort" | Txt]} ->
			sort(Pid, Irc, Msg, Dst, Nick, Txt),
			loop();
		{ act, Pid, Irc, Msg, Dst, Nick, ["tail" | Txt]} ->
			tail(Pid, Irc, Msg, Dst, Nick, Txt),
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
						"[\"echo\",  | Txt] -> print Txt",
						"[\"head\",  | Txt] -> first item",
						"[\"last\",  | Txt] -> last item",
						"[\"md5\",   | Txt] -> md5 checksum Txt",
						"[\"rep\", N | Txt] -> repeat Txt N times",
						"[\"sort\"   | Txt] -> sort Txt",
						"[\"tail\",  | Txt] -> opposite of head",
						"[\"wc\"     | Txt] -> word count"
					]
				] },
			loop()
	end.

echo(_, _, _, _, _, []) -> nil;
echo(Pid, _, Msg, Dst, Nick, Txt) ->
	Res = util:j(Txt),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Res)}.

head(_, _, _, _, _, []) -> nil;
head(Pid, _, Msg, Dst, Nick, Txt) ->
	Res = hd(Txt),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Res)}.

last(_, _, _, _, _, []) -> nil;
last(Pid, _, Msg, Dst, Nick, Txt) ->
	Res = lists:last(Txt),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Res)}.

md5(_, _, _, _, _, []) -> nil;
md5(Pid, _, Msg, Dst, Nick, _) ->
	In = string:substr(Msg#ircmsg.rawtxt, length("md5")+1),
	In2 = util:ltrim(In),
	L = binary_to_list(erlang:md5(In2)),
	Hex = [ util:hex(X, 2) || X <- L ],
	Res = util:join("", Hex),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Res)}.

rep(_, _, _, _, _, _, []) -> nil;
rep(Pid, _, Msg, Dst, Nick, N, Txt) when N > 0 ->
	Txt2 = util:j(Txt),
	Res = util:join("", lists:duplicate(N, Txt2)),
	Res2 = ircutil:dotdotdot(Res, 200),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Res2)};
rep(_, _, _, _, _, _, _) -> nil.

sort(Pid, _, Msg, Dst, Nick, ["-r" | Txt]) ->
	Res = util:j(lists:reverse(lists:sort(Txt))),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Res)};
sort(Pid, _, Msg, Dst, Nick, Txt) ->
	Res = util:j(lists:sort(Txt)),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Res)}.

tail(_, _, _, _, _, []) -> nil;
tail(Pid, _, Msg, Dst, Nick, Txt) ->
	Res = util:j(tl(Txt)),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Res)}.

wc(Pid, _, Msg, Dst, Nick, Txt) ->
	Cnt = length(Txt),
	Res = integer_to_list(Cnt),
	Pid ! {pipe, Msg, irc:resp(Dst, Nick, Res)}.

