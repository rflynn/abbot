% ex: set ts=2 noet:
% $Id$
% top-level bot loop; should be fairly generic

-module(bot).
-author("pizza@parseerror.com").
-export([conn/2, conn/1, loop/1, q/2]).
-define(master, "pizza_").
-define(nick, "mod_pizza").
-define(chan, "#mod_spox").

-include_lib("irc.hrl").
-import(irc).
-import(react).

% Connect to an IRC host:port. TCP line-based.
conn(Host, Port) ->
	test(),
	Irc = #ircconn{
		host=Host, port=Port, key=Host, server="blah", real="blah", master="pizza_",
		user=#ircsrc{
			nick=?nick, user="blah", host="blah"}},
	case gen_tcp:connect(Host, Port, [{packet, line}]) of
		{ok, Sock} ->
			io:format("Connected~n"),
			Irc2 = Irc#ircconn{sock=Sock},
			nick(Irc2),
			bot:loop(Irc2);
		{error, Why} ->
			io:format("Error: ~s~n", [Why]),
			conn(Host, Port) % try harder. try again.
	end.

% Handle lines from Sock.
loop(Irc) ->
	receive
		{tcp, Sock, Data} ->
			io:format("<<< ~s", [Data]),
			Irc2 = Irc#ircconn{sock=Sock},
			Irc3 = react(Irc2, irc:parse(Irc, Data)),
			Irc4 = dequeue(Irc3, Irc3#ircconn.q),
			bot:loop(Irc4); % allow module hot-swapping
		quit ->
			io:format("[~w] Exiting...~n", [Irc#ircconn.sock]),
			gen_tcp:close(Irc#ircconn.sock),
			exit(stopped)
	end.

% process each line of irc input
react(Irc, #ircmsg{type="PRIVMSG"}=Msg) ->
	Irc2 = react:privmsg(Irc, Msg),
	Irc2;
react(Irc, #ircmsg{type="PING", src=Src}) -> % PING -> PONG
	send(Irc, #ircmsg{type="PONG", rawtxt=Src#ircsrc.raw}),
	Irc;
react(Irc, #ircmsg{type="376"}) -> % end of MOTD
	send(Irc, #ircmsg{type="JOIN", rawtxt=?chan}),
	Irc;
react(Irc, #ircmsg{type="433"}) -> % nick already in use
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

q(Irc, []) -> Irc;

q(Irc, [#ircmsg{}=Head|Tail]) ->
	io:format("que ~s", [irc:str(Head)]),
	NewQ = Irc#ircconn.q ++ [Head],
	Irc2 = Irc#ircconn{q=NewQ},
	q(Irc2, Tail);

q(Irc, #ircmsg{}=Msg) ->
	io:format("que ~s", [irc:str(Msg)]),
	NewQ = Irc#ircconn.q ++ [Msg],
	Irc#ircconn{q=NewQ}.

dequeue(Irc, [H|T]) ->
	io:format("deq ~s", [irc:str(H)]),
	send(Irc, H),
	Irc#ircconn{q=T};
dequeue(Irc, []) ->
	%io:format("deq QUEUE EMPTY~n"),
	Irc.

% gen_tcp:send wrapper for #ircmsg
send(Irc, Msg) ->
	Data = irc:str(Msg),
	io:format(">>> ~s", [Data]),
	gen_tcp:send(Irc#ircconn.sock, Data).

conn(Host) ->
	conn(Host, irc:default_port()).

% run unit tests from all our modules
% if any fails we should crash
test() ->
	case (
		util:test() and
		erl:test() and
		react:test()
		) of
		true -> true;
		false ->
			io:format("Unit tests failed, halting.~n"),
			halt(),
			false
		end.

