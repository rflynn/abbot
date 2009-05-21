% ex: set ts=2 noet:
% $Id$

-module(bot).
-author("pizza@parseerror.com").
-export([connect/2, connect/1, loop/1]).
-define(nick, "mod_pizza").
-define(chan, "#mod_spox").

-include_lib("irc.hrl").
-import(irc).

% Connect to an IRC host:port. TCP option provides line-based input.
connect(Host, Port) ->
	Me = #ircsrc{nick=?nick, user="blah", host="blah"},
	Irc = #ircconn{host=Host, port=Port, key=Host, user=Me, server="blah", real="blah"},
	case gen_tcp:connect(Host, Port, [{packet, line}]) of
		{ok, Sock} ->
			io:format("Connected~n"),
			Irc2 = Irc#ircconn{sock=Sock},
			send(Irc2, irc:nick(Irc2)),
			send(Irc2, irc:user(Irc2)),
			bot:loop(Irc2);
		{error, Why} ->
			io:format("Error: ~s~n", [Why]),
			connect(Host, Port) % try harder. try again.
	end.

connect(Host) ->
	connect(Host, irc:default_port()).

% Now that we're connected, receive TCP messages and parse them.
loop(Irc) ->
	receive
		{tcp, Sock, Data} ->
			io:format("[~w] Recv: ~s", [Sock, Data]),
			Irc2 = Irc#ircconn{sock=Sock},
			react(Irc2, irc:parse(Irc, Data)),
			bot:loop(Irc2); % call explicitly by module to allow module hot-swapping
		quit ->
			io:format("[~w] Received quit message, exiting...~n", [Irc#ircconn.sock]),
			gen_tcp:close(Irc#ircconn.sock),
			exit(stopped)
	end.

% process each line of irc input
react(Irc, #ircmsg{type="PRIVMSG", dst=Dst, src=#ircsrc{nick=Nick}, txt=Txt}) ->
	case lists:nth(1, Txt) == (Irc#ircconn.user)#ircsrc.nick of
		true ->
			send(Irc, irc:respond(Dst, Nick, "Wassup, " ++ Nick ++ "?"));
		false -> 0
	end;
% PING -> PONG
react(Irc, #ircmsg{type="PING", src=Src}) ->
	send(Irc, #ircmsg{type="PONG", rawtxt=Src#ircsrc.raw});
% "376" indicates End of MOTD.
react(Irc, #ircmsg{type="376"}) ->
	send(Irc, #ircmsg{type="JOIN", rawtxt=?chan});
react(_, _) ->
	io:write("no match.").

% gen_tcp:send wrapper for #ircmsg
send(#ircconn{}=Irc, #ircmsg{}=Msg) ->
	Data = irc:assemble(Msg),
	io:format("[~w] Send: ~s", [Irc#ircconn.sock, Data]),
	gen_tcp:send(Irc#ircconn.sock, Data).

