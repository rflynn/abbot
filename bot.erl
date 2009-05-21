% ex: set ts=2 noet:
% $Id$

-module(bot).
-author("pizza@parseerror.com").
-export([connect/2, connect/1, loop/1]).
-define(nickname, "mod_pizza").
-define(channel, "#mod_spox").

-include_lib("irc.hrl").
-import(irc).

% Connect to an IRC host:port. TCP option provides line-based input.
connect(Host, Port) ->
	case gen_tcp:connect(Host, Port, [{packet, line}]) of
		{ok, Sock} ->
			io:format("Connected~n"),
			send(Sock, #ircmsg{type="NICK", src=?nickname}),
			send(Sock, #ircmsg{type="USER", src=?nickname, txt=["blah","blah","blah","blah"]}),
			loop(Sock);
		{error, Why} ->
			io:format("Error: ~s~n", [Why]),
			connect(Host, Port) % try harder. try again.
	end.

connect(Host) ->
	connect(Host, irc:default_port()).

% Now that we're connected, receive TCP messages and parse them.
loop(Sock) ->
	receive
		{tcp, Sock, Data} ->
			io:format("[~w] Recv: ~s", [Sock, Data]),
			Msg = irc:parse(Data),
			react(Sock, Msg),
			bot:loop(Sock);
		quit ->
			io:format("[~w] Received quit message, exiting...~n", [Sock]),
			gen_tcp:close(Sock),
			exit(stopped)
	end.

% process each line of irc input
react(Sock, #ircmsg{type="PRIVMSG", dst=Dst, src=Src, txt=[?nickname|_]}) ->
	Nick = lists:nth(1, string:tokens(Src, "!")),
	send(Sock, irc:privmsg(Dst, "Wassup, " ++ Nick ++ "?"));

% PING -> PONG
react(Sock, #ircmsg{type="PING", src=Src}) ->
	send(Sock, #ircmsg{type="PONG", rawtxt=Src});

% "376" indicates End of MOTD.
react(Sock, #ircmsg{type="376"}) ->
	send(Sock, #ircmsg{type="JOIN", rawtxt=?channel});

react(_, _) ->
	0.

% gen_tcp:send wrapper for #ircmsg
send(Sock, Msg=#ircmsg{}) ->
	Data = irc:assemble(Msg),
	io:format("[~w] Send: ~s", [Sock, Data]),
	gen_tcp:send(Sock, Data).

