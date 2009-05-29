% ex: set ts=2 noet:
% $Id$
% top-level bot loop; should be fairly generic

-module(bot).
-author("pizza@parseerror.com").
-export(
	[
		conn/2, conn/1, loop/1,
		q/2, deq/2, deqt/1,
		nick/1, newnick/1,
		test/0
	]).
-define(master, "pizza__").
-define(nick, "abbot").
-define(chan, "#mod_spox").
-define(burstlines, 3).
-define(burstsec, 5).

-include_lib("irc.hrl").
-import(irc).
-import(react).
-import(test).

% Connect to an IRC host:port. TCP line-based.
conn(Host, Port) ->
	unit_test(), % all unit tests run at startup, every time.
	Irc = #ircconn{
		host=Host, port=Port, key=Host, server="blah",
		real="blah", master=?master,
		state=irc_state_load(),
		user=#ircsrc{
			nick=?nick, user="blah", host="blah"}},
	case gen_tcp:connect(Host, Port, [{packet, line}]) of
		{ok, Sock} ->
			io:format("Connected~n"),
			Irc2 = Irc#ircconn{sock=Sock},
			Irc3 = nick(Irc2),
			Deqt = timer:apply_interval(1000, bot, deqt, [self()]),
			Irc4 = Irc3#ircconn{deqt=Deqt},
			bot:loop(Irc4);
		{error, Why} ->
			io:format("Error: ~s~n", [Why]),
			conn(Host, Port) % try harder. try again.
	end.

% handle lines from Sock,
% signals from deq timer
loop(Irc) ->
	receive
		deq ->
			Irc2 = bot:deq(Irc, Irc#ircconn.q),
			bot:loop(Irc2);
		save ->
			irc_state_save(Irc),
			bot:loop(Irc);
		{tcp, Sock, Data} ->
			io:format("<<< ~s", [Data]),
			Irc2 = Irc#ircconn{sock=Sock},
			Msg = irc:parse(Irc2, Data),
			Irc3 = ircin(Irc2, Msg),
			Irc4 = react(Irc3, Msg), % possibly queued data
			bot:loop(Irc4);
		{tcp_closed, _Sock} ->
			irc_state_save(Irc),
			io:format("closed!~n");
		{tcp_error, _Sock, Why} ->
			irc_state_save(Irc),
			io:format("error: ~s!~n", [Why]);
		quit ->
			irc_state_save(Irc),
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
	bot:q(Irc, #ircmsg{type="PONG", rawtxt=Src#ircsrc.raw});
react(Irc, #ircmsg{type="376"}) -> % end of MOTD
	bot:q(Irc, #ircmsg{type="JOIN", rawtxt=?chan});
react(Irc, #ircmsg{type="433"}) -> % nick already in use
	NewNick = newnick((Irc#ircconn.user)#ircsrc.nick),
	io:format("Switching nick to ~s...~n", [NewNick]),
	NewUser = (Irc#ircconn.user)#ircsrc{nick=NewNick},
	Irc2 = Irc#ircconn{user=NewUser},
	bot:nick(Irc2);
react(Irc, #ircmsg{type="ERROR", src=_Src}) -> % Oh noes...
	Irc;
react(Irc, _) ->
	Irc.

% queue an #ircmsg{} for sending
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

% signal loop to dequeue output at a regular interval
deqt(Loop_Pid) ->
	Loop_Pid ! deq.

% called regularly to dequeue irc output.
% implements bursting cfged via ircconn ircqopt{}
deq(Irc, []) ->
	Irc;
deq(Irc, [_|_]=Q) ->
	St = irc:state(Irc, deq,
		#ircqopt{
			burstlines = ?burstlines,
			burstsec = ?burstsec
		}),
	MaxBurst = util:min(length(Q), St#ircqopt.burstlines),
	Now = erlang:universaltime(),
	Burst = % has it been long enough since last burst?
		case util:utime_diffsec(St#ircqopt.lastburst, Now) >= St#ircqopt.burstsec of
			true -> MaxBurst; % burst away
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
	Irc2 = send(Irc, H),
	Irc3 = Irc2#ircconn{q=T},
	sendburst(Irc3, Cnt - 1).

% wrapper for all incoming messages
ircin(Irc, #ircmsg{}=Msg) ->
	irctypecnt(Irc, Msg).

% identify ourselves to the network
nick(Irc) ->
	bot:q(Irc, [ irc:nick(Irc),
	             irc:user(Irc) ]).

% gen_tcp:send wrapper for #ircmsg
send(Irc, Msg) ->
	Data = irc:str(Msg),
	io:format(">>> ~s", [Data]),
	gen_tcp:send(Irc#ircconn.sock, Data),
	ircout(Irc, Msg).

% wrapper for all outgoing messages
ircout(Irc, #ircmsg{}=Msg) ->
	irctypecnt(Irc, Msg).

% connect with default irc port; convenience wrapper
conn(Host) ->
	conn(Host, irc:default_port()).

% increment type by count
irctypecnt(Irc, #ircmsg{type=Type}=_Msg) ->
	D = irc:state(Irc, irctype, dict:new()),
	D2 = dict:update_counter(Type, 1, D),
	irc:setstate(Irc, irctype, D2).

newnick([]) ->
	"_";
newnick(OldNick) ->
	Rev = lists:reverse(OldNick),
	{SufR, PreR} =
		lists:splitwith(
			fun(C)-> char:isdigit(C) end, Rev),
	Suf = lists:reverse(SufR),
	Pre = lists:reverse(PreR),
	Suf2 =
		if
			[] == Suf -> 1;
			true -> list_to_integer(Suf) + 1
		end,
	Pre ++ integer_to_list(Suf2).

test_newnick() ->
	[
		{ [ "" ], "_" },
		{ [ "_" ], "_1" },
		{ [ "_1" ], "_2" },
		{ [ "0" ], "1" },
		{ [ "5a5" ], "5a6" },
		{ [ "abbot" ], "abbot1" },
		{ [ "abbot1" ], "abbot2" },
		{ [ "abbot10" ], "abbot11" },
		{ [ "abbot99" ], "abbot100" },
		{ [ "abbot_99" ], "abbot_100" }
	].

irc_state_load() ->
	case file:read_file("store/Irc.state") of
		{ok, Binary} -> binary_to_term(Binary);
		{error, Why} ->
			io:format("Error loading state: ~p~n", [Why]),
			dict:new()
		end.

irc_state_save(Irc) ->
	util:ensure_dir("store"),
	State = Irc#ircconn.state,
	Bytes = term_to_binary(State),
	case file:write_file("store/Irc.state", Bytes) of
		ok -> true;
		{error, Why} ->
			io:format("Error saving state: ~p~n", [Why]),
			false
		end.

% run unit tests from all our modules
% if any fails we should crash
unit_test() ->
	case (
		util:test() and
		erl:test() and
		ruby:test() and
		ircutil:test() and
		react:test() and
		bot:test()
		) of
		true -> true;
		false ->
			io:format("Unit tests failed, exiting.~n"),
			exit(0),
			false
		end.

test() ->
	test:unit(bot,
		[
			{ newnick, test_newnick() }
		]).

