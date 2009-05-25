% ex: set ts=2 noet:
% $Id$
% top-level bot loop; should be fairly generic

-module(bot).
-author("pizza@parseerror.com").
-export([conn/2, conn/1, loop/1, q/2, deqt/1]).
-define(master, "pizza_").
-define(nick, "mod_pizza").
-define(chan, "#mod_spox").
-define(burstlines, 3).
-define(burstsec, 5).

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
			Deqt = timer:apply_interval(1000, bot, deqt, [self()]),
			Irc3 = Irc2#ircconn{deqt=Deqt},
			bot:loop(Irc3);
		{error, Why} ->
			io:format("Error: ~s~n", [Why]),
			conn(Host, Port) % try harder. try again.
	end.

% signal loop to dequeue output at a regular interval
deqt(Loop_Pid) ->
	Loop_Pid ! deq.

% calculate the difference in seconds between two
% erlang:universaltime() calls
% NOTE: there are tons of special cases where this
% will produce the wrong answer, oh well, close enough.
utime_diffsec({{Y1,M1,D1},{H1,I1,S1}}=_Then,
							{{Y2,M2,D2},{H2,I2,S2}}=_Now) ->
	% {{2009,5,25},{4,58,17}}
	((S2 - S1) +
	((I2 - I1) * 60) +
	((H2 - H1) * 60 * 60) +
	((D2 - D1) * 60 * 60 * 24) +
	((M2 - M1) * 60 * 60 * 24 * 31) +
	((Y2 - Y1) * 60 * 60 * 24 * 365)).

% called regularly to dequeue irc output.
% implements bursting as configured via the ircconn ircqopt{} structure
deq(Irc, []) ->
	Irc;
deq(Irc, [_|_]=Q) ->
	St = irc:state(Irc, deq,
		#ircqopt{
			burstlines = ?burstlines,
			burstsec = ?burstsec
		}),
	MaxBurst = util:min(length(Q), St#ircqopt.burstlines), % how many lines we can send now
	Now = erlang:universaltime(),
	Burst = % has it been long enough since we sent the last burst to send another?
		case utime_diffsec(St#ircqopt.lastburst, Now) >= St#ircqopt.burstsec of
			true -> MaxBurst;
			false -> 1 % hasn't been long enough 
			end,
	Irc2 = sendburst(Irc, Burst),
	St2 =
		if
			Burst > 1 -> St#ircqopt{lastburst=Now};
			true -> St
			end,
	irc:setstate(Irc2, deq, St2).

% send an immediate burst of 'Cnt' msgs and update queue
sendburst(Irc, 0) ->
	Irc;
sendburst(Irc, Cnt) ->
	[H|T] = Irc#ircconn.q,
	io:format("deq ~s", [irc:str(H)]),
	send(Irc, H),
	Irc2 = Irc#ircconn{q=T},
	sendburst(Irc2, Cnt - 1).
	
% handle lines from Sock,
% signals from deq timer
loop(Irc) ->
	receive
		deq ->
			Irc2 = deq(Irc, Irc#ircconn.q),
			bot:loop(Irc2);
		{tcp, Sock, Data} ->
			io:format("<<< ~s", [Data]),
			Irc2 = Irc#ircconn{sock=Sock},
			Irc3 = react(Irc2, irc:parse(Irc, Data)), % possibly queued data
			bot:loop(Irc3);
		{tcp_closed, _Sock} ->
			io:format("closed!~n");
		{tcp_error, _Sock, Why} ->
			io:format("error: ~s!~n", [Why]);
		quit ->
			Sock = Irc#ircconn.sock,
			Deqt = Irc#ircconn.deqt,
			io:format("[~w] Cancelling timer ~w...~n", [Sock, Deqt]),
			timer:cancel(Deqt),
			io:format("[~w] Exiting...~n", [Sock]),
			gen_tcp:close(Sock),
			exit(stopped)
	end.

% process each line of irc input
react(Irc, #ircmsg{type="PRIVMSG"}=Msg) ->
	react:privmsg(Irc, Msg);
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
	Irc.

% identify ourselves to the network
nick(Irc) ->
	send(Irc, irc:nick(Irc)),
	send(Irc, irc:user(Irc)).

% queue an #ircmsg for sending
q(Irc, []) ->
	Irc;
q(Irc, [#ircmsg{}=Head|Tail]) ->
	io:format("que ~s", [irc:str(Head)]),
	NewQ = Irc#ircconn.q ++ [Head],
	Irc2 = Irc#ircconn{q=NewQ},
	q(Irc2, Tail);
q(Irc, #ircmsg{}=Msg) ->
	io:format("que ~s", [irc:str(Msg)]),
	NewQ = Irc#ircconn.q ++ [Msg],
	Irc2 = Irc#ircconn{q=NewQ},
	Irc2.

% gen_tcp:send wrapper for #ircmsg
send(Irc, Msg) ->
	Data = irc:str(Msg),
	io:format(">>> ~s", [Data]),
	gen_tcp:send(Irc#ircconn.sock, Data).

% connect with default irc port; convenience wrapper
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

