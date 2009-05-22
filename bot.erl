% ex: set ts=2 noet:
% $Id$

-module(bot).
-author("pizza@parseerror.com").
-export([connect/2, loop/1]).
-define(nickname, "mod_pizza").
-define(channel, "#mod_spox").

-include_lib("irc.hrl").
-import(irc).

% Connect to an IRC server with a given Host and Port.  Set up the TCP option to
% give us messages on a line-by-line basis.
connect(Host, Port) ->
	{ok, Sock} = gen_tcp:connect(Host, Port, [{packet, line}]),
	% According to RFC1459, we need to tell the server our nickname and username
	gen_tcp:send(Sock, "NICK " ++ ?nickname ++ "\r\n"),
	gen_tcp:send(Sock, "USER " ++ ?nickname ++ " blah blah blah blah\r\n"),
	loop(Sock).

% Now that we're connected, receive TCP messages and parse them.
loop(Sock) ->
	receive
		{tcp, Sock, Data} ->
			io:format("[~w] Recv: ~s", [Sock, Data]),
			%parse_line(Sock, string:tokens(Data, ": ")),
			Msg = irc:parse(Data),
			react(Sock, Msg),
			loop(Sock);
		quit ->
			io:format("[~w] Received quit message, exiting...~n", [Sock]),
			gen_tcp:close(Sock),
			exit(stopped)
	end.

send(Sock, Data=#ircmsg{}) ->
	io:format("[~w] Send: ~s", [Sock, Data]),
	gen_tcp:send(Sock, Data).

% The following is an example of the message this fun intends to parse.  Here we see
% the limitation that tokenizing the string on both :'s and spaces puts on us.
% [#Port<0.124>] Received: :jroes!jroes@mask-2EDB8BDB.net PRIVMSG #jroes-test :jroes-test: wassup?
react(Sock, #ircmsg{ type="PRIVMSG", txt=[?nickname|_] } = Msg) ->
	Nick = lists:nth(1, string:tokens(Msg#ircmsg.src, "!")),
	privmsg(Sock, Msg#ircmsg.dst, "You talkin to me, " ++ Nick ++ "?");

% If the second token is "376", then join our channel.  376 indicates End of MOTD.
react(Sock, #ircmsg{type="376"}) ->
%react(Sock, Msg) when Msg#ircmsg.type == "376" ->
	send(Sock, "JOIN :" ++ ?channel ++ "\r\n");

% The server will periodically send PINGs and expect you to PONG back to make sure
% you haven't lost the connection.
react(Sock, #ircmsg{type="PING", rawtxt=Raw}) ->
	send(Sock, "PONG " ++ Raw ++ "\r\n");

react(_, _) ->
	io:write(io:format("no match.~n")).

% This just helps us write a PRIVMSG back to a client without having to type
% the newlines and :'s ourselves so much.  It'll be more useful later.
privmsg(Sock, To, Message) ->
	send(Sock, "PRIVMSG " ++ To ++ " :" ++ Message ++ "\r\n").

