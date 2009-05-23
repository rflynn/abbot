% ex: set ts=2 noet:
% $Id$

-module(bot).
-author("pizza@parseerror.com").
-export([connect/2, connect/1, loop/1, queue/2]).
-define(nick, "mod_pizza").
-define(chan, "#mod_spox").

-include_lib("irc.hrl").
-import(irc).
-import(react).

% Connect to an IRC host:port. TCP option provides line-based input.
connect(Host, Port) ->
	Irc = #ircconn{host=Host, port=Port, key=Host, server="blah", real="blah",
		user=#ircsrc{nick=?nick, user="blah", host="blah"}},
	case gen_tcp:connect(Host, Port, [{packet, line}]) of
		{ok, Sock} ->
			io:format("Connected~n"),
			Irc2 = Irc#ircconn{sock=Sock},
			nick(Irc2),
			bot:loop(Irc2);
		{error, Why} ->
			io:format("Error: ~s~n", [Why]),
			connect(Host, Port) % try harder. try again.
	end.

% Now that we're connected, receive TCP messages and parse them.
loop(Irc) ->
	receive
		{tcp, Sock, Data} ->
			io:format("<<< ~s", [Data]),
			Irc2 = Irc#ircconn{sock=Sock},
			Irc3 = react(Irc2, irc:parse(Irc, Data)),
			Irc4 = dequeue(Irc3, Irc3#ircconn.q),
			bot:loop(Irc4); % by module to allow module hot-swapping
		quit ->
			io:format("[~w] Exiting...~n", [Irc#ircconn.sock]),
			gen_tcp:close(Irc#ircconn.sock),
			exit(stopped)
	end.

% process each line of irc input
react(Irc, #ircmsg{type="PRIVMSG"}=Msg) ->
	Irc2 = react:privmsg(Irc, Msg),
	Irc2;
react(Irc, #ircmsg{type="PING", src=Src}) ->
	% PING -> PONG
	send(Irc, #ircmsg{type="PONG", rawtxt=Src#ircsrc.raw}),
	Irc;
react(Irc, #ircmsg{type="376"}) ->
	% 376 -> end of MOTD
	send(Irc, #ircmsg{type="JOIN", rawtxt=?chan}),
	Irc;
react(Irc, #ircmsg{type="433"}) ->
	% 433 -> nick already in use
	NewNick = (Irc#ircconn.user)#ircsrc.nick ++ "_",
	io:format("Switching nick to ~s...~n", [NewNick]),
	NewUser = (Irc#ircconn.user)#ircsrc{nick=NewNick},
	Irc2 = Irc#ircconn{user=NewUser},
	nick(Irc2),
	Irc2;
react(Irc, _) ->
	%io:format("no match.~n"),
	Irc.

nick(Irc) ->
	send(Irc, irc:nick(Irc)),
	send(Irc, irc:user(Irc)).

queue(Irc, #ircmsg{}=Msg) ->
	%io:format("que ~s", [irc:assemble(Msg)]),
	NewQ = if
		is_list(Irc#ircconn.q) -> Irc#ircconn.q ++ Msg;
		true -> [ Irc#ircconn.q ] ++ Msg
	end,
	Irc#ircconn{q=NewQ}.

dequeue(Irc, [H|T]) ->
	%io:format("deq ~s", [irc:assemble(H)]),
	send(Irc, H),
	Irc#ircconn{q=T};

dequeue(Irc, H) when H /= [] ->
	%io:format("deq ~s", [irc:assemble(H)]),
	send(Irc, H),
	Irc#ircconn{q=[]};

dequeue(Irc, _) ->
	%io:format("deq NO MATCH, WTF~n"),
	Irc.

% gen_tcp:send wrapper for #ircmsg
send(Irc, Msg) ->
	Data = irc:assemble(Msg),
	io:format(">>> ~s", [Data]),
	gen_tcp:send(Irc#ircconn.sock, Data).

connect(Host) ->
	connect(Host, irc:default_port()).

