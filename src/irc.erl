% ex: set ts=2 noet:
% $Id$
% generic irc protocol structures, serializer/unseralizers

-module(irc).
-author("pizza@parseerror.com").
-export([
	state/3, setstate/3,
	parse/2, msgparse/1, str/1,
	nick/1, user/1, privmsg/2, resp/3,
	action/1, master/1,
	is_chan/1, default_port/0,
	test/0
	]).
-include_lib("irc.hrl").

state(Irc, Key, Else) ->
	% lookup entry in state dict by key
	State = Irc#ircconn.state,
	case dict:find(Key, State) of
		error -> Else;
		{ok, X} -> X
	end.

setstate(Irc, Key, Val) ->
	St = dict:store(Key, Val, Irc#ircconn.state),
	Irc#ircconn{state=St}.

master(Irc) ->
	Irc#ircconn.master.

% break a line of IRC input into an #ircmsg
parse(Irc, Str) ->
	Msg = msgparse(Str),
	msgirc_(Irc, Msg).

msgparse(Str) ->
	% TODO: cleanup so we process an irc message in the simplest possible way
	Trim = lists:flatten(string:tokens(Str, "\r\n")),
	Split = util:tokens(Trim, ": ", 4), % split line into consituent parts
	Str2 = ircutil:stripjunk([Trim]),
	Split2 = ircutil:stripjunk(Split),
	io:format("Split2=~p Str2=~p~n", [Split2,Str2]),
	parse_(Split2, Str2).

% guts of parse
parse_(["PING"=Type, Src], Raw) ->
	msg_(Src, Type, "", [], Raw);
parse_(["ERROR"=Src, Type, Dst | Txt], Raw) ->
	msg_(Src, Type, Dst, Txt, Raw);
parse_([Src, "QUIT"=Type | _], Raw) ->
	msg_(Src, Type, "", [], Raw);
parse_([Src, Type, Dst | Txt], Raw) ->
	msg_(Src, Type, Dst, Txt, Raw).

% ircmsg wrapper
msg_(Src, Type, Dst, Txt, Raw) ->
	RealTxt =
		if
			length(Txt) > 0 ->
				Txt2 = util:nth(1, Txt, ""),
				Txt3 = string:tokens(Txt2, " "),
				[First|Rest] = Txt3,
				% trim leading and following : and trailing , from first term, if it exists
				First2 = util:trim(First, $:),
				First3 = util:rtrim(First2, $,),
				[ First3 ] ++ Rest;
			true -> []
		end,
	R = util:tokens(lists:flatten(Raw), ":", 2),
	Rawtxt = util:nth(2, R, ""),
	io:format("msg_ Raw=~p~n", [Raw]),
	io:format("msg_ Txt=~p~n", [Txt]),
	io:format("msg_ RealTxt=~p~n", [RealTxt]),
	io:format("msg_ Rawtxt=~p~n", [Rawtxt]),
	#ircmsg{
		type 		= Type,
		src  		= srcparse(Src),
		dst  		= Dst,
		txt  		= RealTxt,
		rawtxt 	= Rawtxt,
		raw  		= Raw
	}.

% ircmsg wrapper
msgirc_(Irc, Msg) ->
	Msg#ircmsg{
		host = Irc#ircconn.key,
		user = (Irc#ircconn.user)#ircsrc.nick
	}.

% parse an irc "source", i.e. message sender, into an #ircsrc{}
srcparse(Src) ->
	Tok = string:tokens(Src, "!@"),
	case length(Tok) of
		3 -> % user
			Nick = hd(Tok),
			User = lists:nth(2, Tok),
			Host = lists:nth(3, Tok),
			#ircsrc{raw=Src, nick=Nick, user=User, host=Host};
		1 -> % host
			Host = hd(Tok),
			#ircsrc{raw=Src, host=Host};
		_ ->
			#ircsrc{raw=Src}
	end.

% opposite of parse -- transform an #ircmsg into a string suitable for sending
str(#ircmsg{type="PRIVMSG"=Type, dst=Dst, txt=[], rawtxt=Rawtxt}) ->
	Type ++ " " ++ Dst ++ " :" ++ Rawtxt ++ "\r\n";
str(#ircmsg{type="PRIVMSG"=Type, dst=Dst, txt=Txt, rawtxt=""}) ->
	Type ++ " " ++ Dst ++ " :" ++ util:j(Txt) ++ "\r\n";
str(#ircmsg{type="PONG"=Type, rawtxt=Raw}) ->
	Type ++ " " ++ Raw ++ "\r\n";
str(#ircmsg{type="JOIN"=Type, rawtxt=Rawtxt}) ->
	Type ++ " :" ++ Rawtxt ++ "\r\n";
str(#ircmsg{type="PART"=Type, rawtxt=Rawtxt}) ->
	Type ++ " " ++ Rawtxt ++ "\r\n";
str(#ircmsg{type="NICK"=Type, src=Src}) ->
	Type ++ " " ++ Src ++ "\r\n";
str(#ircmsg{type="USER"=Type, src=Src, txt=Txt}) ->
	Type ++ " " ++ Src ++ " " ++ util:j(Txt) ++ "\r\n";
str(#ircmsg{type="QUIT"=Type, rawtxt=Rawtxt}) ->
	Type ++ " :" ++ Rawtxt ++ "\r\n".

% wrapper for building certain types of #ircmsg{}s

nick(Irc)	->
	#ircmsg{
		type = "NICK",
		src = (Irc#ircconn.user)#ircsrc.nick
	}.

user(Irc)	->
	#ircmsg{
		type = "USER",
		src = (Irc#ircconn.user)#ircsrc.nick,
		txt =	[
			(Irc#ircconn.user)#ircsrc.user,
			(Irc#ircconn.user)#ircsrc.host,
			(Irc#ircconn.server),
			(Irc#ircconn.real)
		]
	}.

privmsg(Dst, Say)	->
	#ircmsg{
		type = "PRIVMSG",
		dst = Dst,
		rawtxt = Say
	}.

% produce the correct response based on the type of message
resp(Dst, Src, Say) ->
	case is_chan(Dst) of
	true  -> privmsg(Dst, Say);
	false -> privmsg(Src, Say)
	end.

action(Str) ->
	"\1" ++ "ACTION" ++ " " ++ Str ++ "\1".

% is a destination a channel?
% used to differentiate between channel messages and user messages
is_chan([$#|_]) ->
	true;
is_chan([$@|_]) ->
	true;
is_chan(_) ->
	false.

default_port() ->
	6667.

% 
test() ->
	test_parse(),
	test_srcparse(),
	true.

% test of parse [{ input, output }]
test_parse() ->
[
	{
		":valhall.no.eu.dal.net NOTICE AUTH :*** Looking up your hostname..."
	},
	{
		":valhall.no.eu.dal.net 001 mod_pizza :Welcome to the DALnet IRC Network mod_pizza!~mod_pizza@12.229.112.195"
	},
	{
		":valhall.no.eu.dal.net 002 mod_pizza :Your host is valhall.no.eu.dal.net, running version bahamut-1.8(06)"
	},
	{
		":valhall.no.eu.dal.net 003 mod_pizza :This server was created Thu Mar 26 2009 at 16:23:13 CET"
	},
	{
		":valhall.no.eu.dal.net 004 mod_pizza valhall.no.eu.dal.net bahamut-1.8(06) aAbcdefFghiIjkKmnoOrRswxXy bceiIjklLmMnoOprRstv"
	},
	{
		":valhall.no.eu.dal.net 005 mod_pizza NETWORK=DALnet SAFELIST MAXBANS=200 MAXCHANNELS=20 CHANNELLEN=32 ...:20 PREFIX=(ov)@+ STATUSMSG=@+ :are available on this server"
	},
	{
		":valhall.no.eu.dal.net 251 mod_pizza :There are 121 users and 25948 invisible on 41 servers"
	},
	{
		":valhall.no.eu.dal.net 376 mod_pizza :End of /MOTD command."
	},
	{
		":mod_pizza MODE mod_pizza :+i"
	},
	{
		"PING :valhall.no.eu.dal.net"
	},
	{
		":spox!~spox@pool-98-108-144-112.ptldor.fios.verizon.net PRIVMSG #mod_spox :lol"
	},
	{
		":spox!~spox@pool-98-108-144-112.ptldor.fios.verizon.net PRIVMSG #mod_spox :mod_pizza, lol"
	},
	{
		":jade.fl.us.dal.net 433 * mod_pizza :Nickname is already in use."
	},
	{
		"ERROR :Closing Link: 0.0.0.0 (Ping timeout)"
	}
].

test_srcparse() ->
[
	{
		"",
		{ircsrc,[],[],[],[]}
	},
	{
		"pizza_!~pizza_@12.229.112.195",
		{ircsrc,"pizza_!~pizza_@12.229.112.195","pizza_","~pizza_",
		        "12.229.112.195"}
	},
	{
 		"spox!~spox@pool-98-108-144-112.ptldor.fios.verizon.net",
		{ircsrc,"spox!~spox@pool-98-108-144-112.ptldor.fios.verizon.net",
        "spox","~spox",
        "pool-98-108-144-112.ptldor.fios.verizon.net"}
	},
	{
		"punch.va.us.dal.net",
		{ircsrc,"punch.va.us.dal.net",[],[],"punch.va.us.dal.net"}
	}
].

