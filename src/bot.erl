% ex: set ts=2 noet:
% $Id$
% top-level bot loop; should be fairly generic

% user-configurable stuff
-define(pass,       "kazaam!").		% password for changing "master" user
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

% top-level launch
% Bot0 = spawn(bot, go, ["irc.dal.net"]).
go(Where) ->
	% trap exits. this means that when a plugin
	% crashes, we catch its exit message and reincarnate it.
	process_flag(trap_exit, true),
	io:format("Loading plugins...~n"),
	Plugins = plugins_load(),
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
		key=Host, server="blah",
		real="blah",
		master=?master, pass=?pass,
		state=irc_state_load(),
		user=#ircsrc{
			nick=?nick, user="blah", host="blah"}},
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
			Irc2 = Irc,
			%Irc2 = bot:reconn(Irc),
			loop(Irc2, Plugins);
		deq ->
			%io:format("loop {deq}~n"),
			Irc2 = bot:deq(Irc, Irc#ircconn.q),
			loop(Irc2, Plugins);
		{q, Msg} ->
			% direct (original) output path, no
			% pipelining
			io:format("loop {q, Msg=~p}~n", [Msg]),
			% plugin req to queue an output msg
			Irc2 = bot:q(Irc, Msg),
			loop(Irc2, Plugins);
		{pipe, Req, Resp} ->
			% alternative path to {q...} that allows piping
			% between commands, i.e. insult | translate en es
			% this chains the output of each plugin into the input of the next
			io:format("loop {pipe, Req=~p Resp=~p}~n", [Req, Resp]),
			Irc2 = pipe_run(Irc, Plugins, Req, Resp),
			loop(Irc2, Plugins);
		{setstate, Key, Val} ->
			io:format("Exiting...~n"),
			% plugin req to update Irc state dict
			io:format("loop {setstate, Key=~p, Val=~p}~n",
				[Key, Val]),
			Irc2 = irc:setstate(Irc, Key, Val),
			loop(Irc2, Plugins);
		{irc, Irc2} ->
			% update IRC object itself, i.e. when
			% changing nick
			io:format("loop {irc, Irc2}~n"),
			loop(Irc2, Plugins);
		save ->
			% serialize and save bot state to disk
			io:format("loop save~n"),
			irc_state_save(Irc),
			loop(Irc, Plugins);
		{'EXIT', Pid, _Why} ->
			% catch plugin reloads/crashes
			Plugins2 = plugin_restart(Plugins, Pid),
			loop(Irc, Plugins2);
		{plugins, reload} ->
			loop(Irc, plugins_load());
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
			irc_state_save(Irc),
			Irc2 = Irc#ircconn{connected=false},
			loop(Irc2, Plugins);
		{tcp_error, _Sock, Why} ->
			io:format("~s:~p err ~s~n",
				[Irc#ircconn.host, Irc#ircconn.port, Why]),
			irc_state_save(Irc),
			Irc2 = Irc#ircconn{connected=false},
			loop(Irc2, Plugins);
		quit ->
			io:format("Exiting...~n"),
			irc_state_save(Irc),
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
	[ First | Rest ] =
		[ util:trim(P) || P <- string:tokens(Rawtxt, "|") ],
	Msg2 = Msg#ircmsg{
		txt = string:tokens(First, " "),
		rawtxt=First,
		pipeline=Rest
	},
	Msg2.

% evaluate a pipelined message; feeding the output
% of each plugin into the input of the next one
% until there are none left, and then output
% the msg
pipe_run(Irc, _, #ircmsg{pipeline=[]}, #ircmsg{}=Resp) ->
	bot:q(Irc, Resp);
pipe_run(Irc, _, _, []) ->
	Irc;
pipe_run(Irc, Plugins, Req, #ircmsg{}=Resp) ->
	[H|T] = Req#ircmsg.pipeline,
	Rawtxt2 = H ++ " " ++ Resp#ircmsg.rawtxt,
	Req2 = Req#ircmsg{
		% pass the output of the response as the suffix
		% of the next request. plugins must implement a command
		% structure that allows this
		rawtxt = Rawtxt2,
		txt = string:tokens(Rawtxt2, " "),
		pipeline = T
	},
	react(Irc, Req2, Plugins);
pipe_run(Irc, Plugins, Req, [Resp|Rest]) ->
	Irc2 = pipe_run(Irc, Plugins, Req, Resp),
	pipe_run(Irc2, Plugins, Req, Rest).


% check that Irc.connected is true, if not, reconnect
reconn(Irc) ->
	io:format("reconn connected=~p~n", [Irc#ircconn.connected]),
	if
		not Irc#ircconn.connected ->
			doreconn(Irc);
		true ->
			Irc
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
	bot:q(Irc2, irc:nick(NewNick));
react(Irc, #ircmsg{type="ERROR", src=_Src}, _) ->
	% Oh noes...
	Irc;
react(Irc, #ircmsg{type="NOTICE"}, _) ->
	Irc;
react(Irc, #ircmsg{type="PART"}, _) ->
	Irc;
react(Irc, #ircmsg{type="JOIN"}, _) ->
	Irc;
react(Irc, #ircmsg{type="QUIT"}, _) ->
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
	First2 = util:rtrim(First, $:),
	First3 = util:rtrim(First2, $,),
	{Msg2, Txt} =
		if % are you talking to me? i don't see anyone else...
			Dst == Me ->
				{Msg, All};
			First3 == Me ->
				% chan privmsg where my nick is the first word
				Msg3 = ircutil:ltrim_nick(Msg, Rawtxt, Me),
				{Msg3, Rest};
			true ->
				{Msg, All}
		end,
	do_privmsg(Plugins, Irc, Msg2, Dst, From, Txt).

do_privmsg(Plugins, Irc, _, Dst, From, ["help" | _]=Txt) ->
	help(Irc, Plugins, Dst, From, Txt);
do_privmsg(Plugins, Irc, Msg2, Dst, From, Txt) ->
	Pid = self(),
	% notify every single plugin about the msg we've received;
	% most of which do absolutely nothing.
	% if they do match some plugin pattern and generate a response
	% or change in IRC state, they can "Pid ! foo", which will
	% be processed in loop()
	[
		PluginPid ! {act, Pid, Irc, Msg2, Dst, From, Txt} ||
		{_Name,  _Atom, PluginPid} <- Plugins
	],
	Irc.


help(Irc, Plugins, Dst, Nick, ["help"]) ->
	% for a generic help message, list all plugins
	PluginNames = lists:sort([ Name || {Name,_,_} <- Plugins ]),
	bot:q(Irc,
		[ irc:resp(Dst, Nick, Nick ++ ": " ++ Out)
			|| Out <-
			[
				"[\"help\" | Topic] -> get help for a particular topic",
				"Topic = [" ++ util:join(",", PluginNames) ++ "]"
			]
		]);
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
	Irc.

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
sendburst(Irc, 0) ->
	Irc;
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

% reconstitute existing, serialized irc state field from file, if it exists
irc_state_load() ->
	case file:read_file("store/Irc.state") of
		{ok, Binary} -> binary_to_term(Binary);
		{error, Why} ->
			io:format("Error loading state: ~p~n", [Why]),
			dict:new()
		end.

% write irc state field to disk for later reconstitution
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

% find, load, test and return all available plugins
plugins_load() ->
	case file:list_dir("plugin") of
		{ok, Files} ->
			Ext = code:objfile_extension(), % ".beam"
			Files2 = lists:filter( % all .beam files
				fun(F) -> string:str(F, Ext) /= 0 end, Files),
			Names = [ % parse prefix
				string:substr(Filename, 1,
					string:str(Filename, Ext) - 1)
						|| Filename <- Files2 ],
			io:format("Plugins=~p~n", [Names]),
			[ plugin_run(Name) || Name <- Names ];
		_ ->
			io:format("'plugin' directory not found. exiting.~n"),
			exit(whereareyou)
		end.

% given a plugin module name as a string,
% load, unit test, spawn process and link plugin
% and return record
plugin_run(Name) ->
	Atom = list_to_atom(Name),
	case Atom:test() of
		false ->
			io:format("Unit tests failed, exiting.~n"),
			exit(0);
		true ->
			Pid = spawn_link(Atom, loop, []),
			{ Name, Atom, Pid }
		end.

% DeadPid crashed, search Plugins for a matching pid and
% replace entry with new instance of plugin
plugin_restart(Plugins, DeadPid) ->
	lists:map(
		fun({Name, _Atom, Pid}=P) ->
			if
				Pid == DeadPid ->
					io:format("Pid ~p was the ~s plugin, restarting...~n",
						[DeadPid, Name]),
					plugin_run(Name);
				true -> P
			end
		end,
		Plugins).

test() ->
	test:unit(bot,
		[
			{ newnick, test_newnick() }
		]).

