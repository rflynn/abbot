% ex: set ts=2 noet:
% $Id$
% top-level bot loop; should be fairly generic

-module(bot).
-author("pizza@parseerror.com").
-export(
	[
		test/0,
		go/1,
		start/2,
		conn/3, loop/2,
		q/2, deq/2, deqt/1,
		newnick/1
	]).

-define(master,     "pizza_").
-define(nick,       "abbot").
-define(chan,       "#mod_spox").
-define(pass,       "foobar").
-define(burstlines, 3).
-define(burstsec,   5).

-include_lib("irc.hrl").
-import(irc).
-import(react).
-import(test).
-import(ircutil).

go(Where) ->
	Plugins = load_plugins(),
	start(Where, Plugins).

%load_plugins() ->
%	[].

load_plugins() ->
	case file:list_dir("plugin") of
	{ok, Files} ->
		Files2 = lists:filter(
			fun(F)-> string:str(F, ".beam") /= 0 end,
				Files),
		[ run_plugin(Name) || Name <- Files2 ];
	_ ->
		io:format("wtf~n"),
		exit("argh")
	end.

% load and unit test a given plugin
run_plugin(Name) ->
	Atom = list_to_atom(Name),
	case	Atom:test() of
		false ->
			io:format("Unit tests failed, exiting.~n"),
			exit(0);
		true ->
			Pid = spawn(Atom, loop, []),
			{ Name, Atom, Pid }
		end.

% given a list of hosts or {host,port} destinations
% and some plugins, launch all the connections
start([], _) ->
	nil;
start([{Host,Port} | Rest], Plugins) ->
	conn(Host, Port, Plugins) ++ start(Rest, Plugins);
start([Host | Rest], Plugins) ->
	conn(Host, 6667, Plugins) ++ start(Rest, Plugins);
start({Host,Port}, Plugins) ->
	conn(Host, Port, Plugins).

% Connect to an IRC host:port. TCP line-based.
conn(Host, Port, Plugins) ->
	Irc = #ircconn{
		host=Host, port=Port, key=Host, server="blah",
		real="blah", master=?master,
		state=irc_state_load(),
		user=#ircsrc{
			nick=?nick, user="blah", host="blah"}},
	case gen_tcp:connect(Host, Port, [{packet, line}]) of
		{ok, Sock} ->
			io:format("Connected~n"),
			Nick = (Irc#ircconn.user)#ircsrc.nick,
			Irc2 = Irc#ircconn{sock=Sock},
			Irc3 = bot:q(Irc2, irc:nick(Nick)),
			Irc4 = bot:q(Irc3, irc:user(Irc3)),
			Deqt =
				timer:apply_interval(1000, bot, deqt, [self()]),
			Irc5 = Irc4#ircconn{deqt=Deqt},
			bot:loop(Irc5, Plugins);
		{error, Why} ->
			io:format("Error: ~s~n", [Why]),
			% TODO: pause before reconnecting
			conn(Host, Port, Plugins) % try harder. try again.
	end.

% API calls to modify the state of the Irc object;
% made from the dequeue timer, from the plugins
% and possibly from the shell
loop(Irc, Plugins) ->
	receive
		deq ->
			Irc2 = bot:deq(Irc, Irc#ircconn.q),
			bot:loop(Irc2, Plugins);
		{q, Msg} ->
			% plugin req to queue an output msg
			Irc2 = bot:q(Irc, Msg),
			bot:loop(Irc2, Plugins);
		{setstate, Key, Val} ->
			% plugin req to update Irc state dict
			Irc2 = irc:setstate(Irc, Key, Val),
			bot:loop(Irc2, Plugins);
		{irc, Irc2} ->
			% update IRC object itself, i.e. when
			% changing nick
			bot:loop(Irc2, Plugins);
		save ->
			% serialize and save bot state to disk
			irc_state_save(Irc),
			bot:loop(Irc, Plugins);
		{tcp, Sock, Data} ->
			% a line of IRC input from socket
			io:format("<<< ~s", [Data]),
			Irc2 = Irc#ircconn{sock=Sock},
			Msg = irc:parse(Irc2, Data),
			Irc3 = ircin(Irc2, Msg),
			Irc4 = react(Irc3, Msg, Plugins),
			bot:loop(Irc4, Plugins);
		{tcp_closed, _Sock} ->
			irc_state_save(Irc),
			% TODO: reconnect ffs
			io:format("closed!~n");
		{tcp_error, _Sock, Why} ->
			irc_state_save(Irc),
			% TODO: reconnect
			io:format("error: ~s!~n", [Why]);
		quit ->
			irc_state_save(Irc),
			Sock = Irc#ircconn.sock,
			Deqt = Irc#ircconn.deqt,
			io:format("[~w] Cancelling timer ~w...~n",
				[Sock, Deqt]),
			timer:cancel(Deqt),
			io:format("[~w] Exiting...~n", [Sock]),
			gen_tcp:close(Sock),
			exit(stopped)
	end.

% input handling; most of the stuff here is IRC
% protocol crap to keep the connection alive and/or
% sane, except...
react(Irc, #ircmsg{type="PRIVMSG"}=Msg, Plugins) ->
	% ... me. I send every privmsg to all our plugins;
	% most of the time resulting in absolutely nothing;
	% however, if something does match a pattern they're
	% looking for and they produce output, they send loop()
	% a {q, ...} signal to queue their output to be sent.
	privmsg(Irc, Msg, Plugins);
react(Irc, #ircmsg{type="PING", src=Src}, _) ->
	% PING -> PONG
	bot:q(Irc, #ircmsg{type="PONG", rawtxt=Src#ircsrc.raw});
react(Irc, #ircmsg{type="376"}, _) ->
	% end of MOTD, let's do stuff!
	bot:q(Irc, #ircmsg{type="JOIN", rawtxt=?chan});
react(Irc, #ircmsg{type="433"}, _) ->
	% nick already in use
	NewNick = newnick((Irc#ircconn.user)#ircsrc.nick),
	io:format("Switching nick to ~s...~n", [NewNick]),
	NewUser = (Irc#ircconn.user)#ircsrc{nick=NewNick},
	Irc2 = Irc#ircconn{user=NewUser},
	bot:nick(Irc2);
react(Irc, #ircmsg{type="ERROR", src=_Src}, _) ->
	% Oh noes...
	Irc;
react(Irc, #ircmsg{type=Type}, _) ->
	% something i didn't expect
	io:format("*** TYPE ~s unhandled~n", [Type]),
	Irc.

% handle a privmsg
privmsg(
	Irc,
	#ircmsg{
		type="PRIVMSG",
		dst=Dst,
		src=#ircsrc{nick=From},
		txt=[First|Rest]=All,
		rawtxt=Rawtxt
	}=Msg,
	Plugins) ->
	Me = (Irc#ircconn.user)#ircsrc.nick,
	{Msg2, Txt} =
		if % are you talking to me? i don't see anyone else...
			Dst == Me ->
				{Msg, All};
			First == Me ->
				% chan privmsg where my nick is the first word
				Msg3 = ircutil:ltrim_nick(Msg, Rawtxt, Me),
				{Msg3, Rest};
			true ->
				{Msg, All}
		end,
		Pid = self(),
		% notify every single plugin about the msg we've received;
		% most of which do absolutely nothing.
		% if they do match some plugin pattern and generate a response
		% or change in IRC state, they can "Pid ! foo", which will
		% be processed in loop()
		[
			PluginPid ! {act, Pid, Irc, Msg2, Dst, From, Txt} ||
			{_Name,  _Atom, PluginPid} <- Plugins
		].

% queue an #ircmsg{} for sending
q(Irc, []) ->
	Irc;
q(Irc, [#ircmsg{}=Head|Tail]) ->
	%io:format("que ~s", [irc:str(Head)]),
	NewQ = Irc#ircconn.q ++ [Head],
	Irc2 = Irc#ircconn{q=NewQ},
	q(Irc2, Tail);
q(Irc, #ircmsg{}=Msg) ->
	%io:format("que ~s", [irc:str(Msg)]),
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
	%io:format("deq ~s", [irc:str(H)]),
	Irc2 = send(Irc, H),
	Irc3 = Irc2#ircconn{q=T},
	sendburst(Irc3, Cnt - 1).

% wrapper for all incoming messages
ircin(Irc, #ircmsg{}=Msg) ->
	irctypecnt(Irc, Msg).

% gen_tcp:send wrapper for #ircmsg
send(Irc, Msg) ->
	Data = irc:str(Msg),
	io:format(">>> ~s", [Data]),
	gen_tcp:send(Irc#ircconn.sock, Data),
	ircout(Irc, Msg).

% wrapper for all outgoing messages
ircout(Irc, #ircmsg{}=Msg) ->
	irctypecnt(Irc, Msg).

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

test() ->
	test:unit(bot,
		[
			{ newnick, test_newnick() }
		]).

