% ex: set ts=2 noet:
% $Id$
% top-level bot loop; should be fairly generic

% user-configurable stuff
-define(pass,       "****!").			% password for changing "master" user
-define(master,     "pizza_").		% default master nick
-define(nick,       "abbot").			% default "base" bot nick
-define(chan,       "#mod_spox").	% default channel

-module(bot).
-author("pizza@parseerror.com").
-export(
	[
		test/0,
		go/1,
		start/2,
		conn/3, loop/2, reconnt/1, reconn/1,
		q/2, deq/2, deqt/1,
		newnick/1
	]).
-define(reconn_interval,	 5000). % reconnect check msecs
% output queue config
-define(deq_interval,  		  900). % line send msec
-define(burstlines, 					3). % max output lines to burst at once
-define(burstsec,   					5). % minimum time between multi-line bursts

-include_lib("irc.hrl").
-import(irc).
-import(react).
-import(test).
-import(ircutil).
-import(plugins).

% top-level launch
% Bot0 = spawn(bot, go, ["irc.dal.net"]).
go(Where) ->
	% trap exits. this means that when a plugin
	% crashes, we catch its exit message and reincarnate it.
	process_flag(trap_exit, true),
	io:format("Loading plugins...~n"),
	Plugins = plugins:load(),
	io:format("Plugins loaded.~n"),
	State = start(Where, Plugins),
	io:format("Started.~n"),
	State.

% given a list of hosts or {host,port} destinations
% and some plugins, launch all the connections
start([], _) ->
	[];
start(Host, Plugins) ->
	io:format("start Host=~p Plugins=~p~n",
		[Host, Plugins]),
	conn(Host, 6667, Plugins).% ++ start(Rest, Plugins).

% one-time connection setup and launch
conn(Host, Port, Plugins) ->
	io:format("conn Host=~s Port=~p~n", [Host, Port]),
	Irc = #ircconn{
		host=Host, port=Port,
		key=Host, server="example.com",
		real=?nick,
		master=?master, pass=?pass,
		state=irc:state_load(),
		user=#ircsrc{
			nick=?nick, user=?nick, host="example.com"}},
	Pid = self(),
	Reconnt = timer:apply_interval(
		?reconn_interval, bot, reconnt, [Pid]),
	Deqt = timer:apply_interval(
		?deq_interval, bot, deqt, [Pid]),
	Irc2 = Irc#ircconn{cont=Reconnt, deqt=Deqt},
	Irc3 = reconn(Irc2),
	loop(Irc3, Plugins).

% signal loop to check connection
reconnt(Loop_Pid) ->
	Loop_Pid ! conn.

% API calls to modify the state of the Irc object;
% made from the dequeue timer, from the plugins
% and possibly from the shell
loop(Irc, Plugins) ->
	receive
		conn ->
			% attempt to connect if we're not already
			%Irc2 = Irc,
			Irc2 = bot:reconn(Irc), % FIXME: does this work or not?
			loop(Irc2, Plugins);
		deq ->
			Irc2 = bot:deq(Irc, Irc#ircconn.q),
			loop(Irc2, Plugins);
		{q, Msg} ->
			% direct (original) output path, no pipelining
			io:format("loop {q, Msg=~p}~n", [Msg]),
			Irc2 = bot:q(Irc, Msg),
			loop(Irc2, Plugins);
		{pipe, Req, Resp} ->
			% output path, allow pipelining
			io:format("loop {pipe, Req=~p Resp=~p}~n", [Req, Resp]),
			Irc2 = pipe_run(Irc, Plugins, Req, Resp),
			loop(Irc2, Plugins);
		{setstate, Key, Val} ->
			% plugin req to update Irc state dict
			if
				% seen's dictionary is too large...
				Key == seen -> io:format("loop {setstate, Key=~p, Val=...}~n", [Key]);
				true -> io:format("loop {setstate, Key=~p, Val=~p}~n", [Key, Val])
				end,
			Irc2 = irc:setstate(Irc, Key, Val),
			loop(Irc2, Plugins);
		{irc, Irc2} ->
			% update IRC object itself, i.e. when changing nick
			io:format("loop {irc, Irc2}~n"),
			loop(Irc2, Plugins);
		save ->
			% serialize and save bot state to disk
			io:format("loop save~n"),
			irc:state_save(Irc),
			loop(Irc, Plugins);
		{'EXIT', Pid, _Why} ->
			% catch plugin reloads/crashes
			Plugins2 = plugins:restart(Plugins, Pid),
			loop(Irc, Plugins2);
		{plugins, reload} ->
			loop(Irc, plugins:load());
		{tcp, Sock, Data} ->
			% a line of IRC input from socket
			io:format("<<< ~s", [Data]),
			Irc2 = Irc#ircconn{sock=Sock},
			Msg = irc:parse(Irc2, Data),
			Msg2 = pipe_parse(Msg),
			Irc3 = react(Irc2, Msg2, Plugins),
			loop(Irc3, Plugins);
		{tcp_closed, _Sock} ->
			io:format("~s:~p closed~n",
				[Irc#ircconn.host, Irc#ircconn.port]),
			irc:state_save(Irc),
			Irc2 = Irc#ircconn{connected=false},
			loop(Irc2, Plugins);
		{tcp_error, _Sock, Why} ->
			io:format("~s:~p err ~s~n",
				[Irc#ircconn.host, Irc#ircconn.port, Why]),
			irc:state_save(Irc),
			Irc2 = Irc#ircconn{connected=false},
			loop(Irc2, Plugins);
		quit ->
			io:format("Exiting...~n"),
			irc:state_save(Irc),
			timer:cancel(Irc#ircconn.deqt),
			timer:cancel(Irc#ircconn.cont),
			gen_tcp:close(Irc#ircconn.sock),
			exit(stopped);
		Foo ->
			io:format("bot loop() uncaught ! ~p~n", [Foo]),
			loop(Irc, Plugins)
	end.

% split Msg's rawtxt member into pipelines, and
% append them to msg's pipeline list
pipe_parse(#ircmsg{rawtxt=[]}=Msg) ->
	Msg;
pipe_parse(#ircmsg{rawtxt=Rawtxt}=Msg) ->
	case string:substr(Rawtxt, 1, 2) of
		% FIXME: special-case skip piping for Haskell source
		"> " ->
			First = Rawtxt,
			Rest = [];
		_ ->
		[ First | Rest ] =
			[ util:trim(P) || P <- util:strsplit(Rawtxt, " | ") ]
		end,
	Msg2 = Msg#ircmsg{
		txt = string:tokens(First, " "),
		rawtxt=First,
		pipeline=Rest
	},
	Msg2.

% evaluate a pipelined message; feeding the output
% of each plugin into input of next until none left, then send
pipe_run(Irc, _, #ircmsg{pipeline=[]}, #ircmsg{}=Resp) ->
	bot:q(Irc, Resp);
pipe_run(Irc, _, _, []) ->
	Irc;
pipe_run(Irc, Plugins, Req, #ircmsg{}=Resp) ->
	[H|T] = Req#ircmsg.pipeline,
	Rawtxt2 = H ++ " " ++ Resp#ircmsg.rawtxt,
	Req2 = Req#ircmsg{
		% pass output of last Resp as next Req suffix.
		% NOTE: plugins must suport this arch, but in erlang it's natural
		rawtxt = Rawtxt2, pipeline = T,
		txt = string:tokens(Rawtxt2, " ") },
	react(Irc, Req2, Plugins);
pipe_run(Irc, Plugins, Req, [Resp|Rest]) ->
	Irc2 = pipe_run(Irc, Plugins, Req, Resp),
	pipe_run(Irc2, Plugins, Req, Rest).

% check that Irc.connected is true, if not, reconnect
reconn(Irc) ->
	%io:format("reconn connected=~p~n", [Irc#ircconn.connected]),
	if
		not Irc#ircconn.connected -> doreconn(Irc);
		true -> Irc
	end.

% attempt reconnect
doreconn(Irc) ->
	Host = Irc#ircconn.host,
	Port = Irc#ircconn.port,
	io:format("~s:~p con~n", [Host, Port]),
	case gen_tcp:connect(Host, Port, [{packet, line}]) of
		{ok, Sock} ->
			io:format("~s:~p connected!~n", [Host, Port]),
			Irc2 = Irc#ircconn{connected=true, sock=Sock},
			Nick = (Irc2#ircconn.user)#ircsrc.nick,
			bot:q(Irc2, [ irc:nick(Nick), irc:user(Irc2) ]);
		{error, Why} ->
			io:format("~s:~p err ~s~n", [Host, Port, Why]),
			Irc
	end.

% input handling; most here is IRC protocol crap, keep connection alive
react(Irc, #ircmsg{type="PRIVMSG"}=Msg, Plugins) ->
	% ...except this, which handles user commands
	privmsg(Irc, Msg, Plugins);
react(Irc, #ircmsg{type="PING", src=Src}, _) ->
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
	bot:q(Irc2, irc:nick(NewNick));
react(Irc, #ircmsg{type="ERROR"}, _)	-> Irc;
react(Irc, #ircmsg{type="NOTICE"}, _) -> Irc;
react(Irc, #ircmsg{type="JOIN"}, _)		-> Irc;
react(Irc, #ircmsg{type="PART"}, _)		-> Irc;
react(Irc, #ircmsg{type="MODE"}, _)		-> Irc;
react(Irc, #ircmsg{type="NICK"}, _)		-> Irc;
react(Irc, #ircmsg{type="QUIT"}, _)		-> Irc;
react(Irc, #ircmsg{type="372"}, _)		-> Irc;
react(Irc, #ircmsg{type=Type}, _) ->
	io:format("*** TYPE ~s unhandled~n", [Type]),
	Irc.

% handle a privmsg
% TODO: this could be cleaned up quite a bit
privmsg(
	Irc,
	#ircmsg{
		dst=Dst,
		src=#ircsrc{nick=From},
		txt=[First|Rest]=All,
		rawtxt=Rawtxt
	}=Msg, Plugins) ->
	Me = (Irc#ircconn.user)#ircsrc.nick,
	First2 = util:rtrim(First, $:),
	First3 = util:rtrim(First2, $,),
	{Msg2, Txt} =
		if % are you talking to me? i don't see anyone else...
			Dst == Me -> {Msg#ircmsg{tome=true}, All};
			First3 == Me -> % chan privmsg where my nick is the first word
				Msg3 = ircutil:ltrim_nick(Msg, Rawtxt, Me),
				{Msg3, Rest};
			true -> {Msg, All}
		end,
	do_privmsg(Plugins, Irc, Msg2, Dst, From, Txt);
privmsg(Irc, #ircmsg{dst=Dst,src=#ircsrc{nick=From},txt=Txt}=Msg, Plugins) ->
	do_privmsg(Plugins, Irc, Msg, Dst, From, Txt).

do_privmsg(_Plugins, Irc, Msg, _Dst, _From, ["VERSION" | _]) ->
%20:47 [DALnet] [ctcp(pizza__)] VERSION
%20:47 [DALnet] pizza__ [~pizza_@12.229.112.195] requested CTCP VERSION from pizza__:
%20:47 [DALnet] CTCP VERSION reply from pizza__: irssi v0.8.10 - running on Linux i686
	bot:q(Irc,
		irc:notice(Irc, Msg, "VERSION", "irssi v0.8.10 - running on Linux i686"));
do_privmsg(Plugins, Irc, _, Dst, From, ["help" | _]=Txt) ->
	help(Irc, Plugins, Dst, From, Txt);
do_privmsg(Plugins, Irc, Msg2, Dst, From, Txt) ->
	Pid = self(),
	% notify every single plugin about the msg we've received;
	% most of which do absolutely nothing.
	% if they do match some plugin pattern and generate a response
	% or change in IRC state, they can "Pid ! foo", which will
	% be processed in loop()
	[ PluginPid ! {act, Pid, Irc, Msg2, Dst, From, Txt} ||
		{_Name,  _Atom, PluginPid} <- Plugins ],
	Irc.

help(Irc, Plugins, Dst, Nick, ["help", Topic]) ->
	% for a specific help topic, ask the plugins themselves
	Pid = self(),
	[
		if
			Name == Topic -> PluginPid ! {help, Pid, Dst, Nick};
			true -> nil
		end
	 || {Name,  _Atom, PluginPid} <- Plugins
	],
	Irc;
help(Irc, Plugins, Dst, Nick, ["help" | _]) ->
	% for a generic help message, list all plugins
	PluginNames = lists:sort([ Name || {Name,_,_} <- Plugins ]),
	bot:q(Irc,
		[ irc:resp(Dst, Nick, Nick ++ ": " ++ Out)
			|| Out <-
			[
				"[\"help\" | Topic] where Topic == any([" ++ util:join(",", PluginNames) ++ "])"
			]
		]).

% queue an #ircmsg{} for sending
q(Irc, []) -> Irc;
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
deqt(Loop_Pid) -> Loop_Pid ! deq.

% called regularly to dequeue irc output.
% implements bursting cfged via ircconn ircqopt{}
deq(Irc, []) -> Irc;
deq(Irc, Q) ->
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
sendburst(Irc, 0) -> Irc;
sendburst(Irc, Cnt) ->
	[H|T] = Irc#ircconn.q,
	%io:format("deq ~s", [irc:str(H)]),
	Irc2 = send(Irc, H),
	Irc3 = Irc2#ircconn{q=T},
	sendburst(Irc3, Cnt - 1).

% gen_tcp:send wrapper for #ircmsg
send(Irc, Msg) ->
	Data = irc:str(Msg),
	io:format(">>> ~s", [Data]),
	gen_tcp:send(Irc#ircconn.sock, Data),
	Irc.

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

test() ->
	test:unit(bot,
		[
			{ newnick, test_newnick() }
		]).

