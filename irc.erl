% ex: set ts=2 noet:
% $Id$

-module(irc).
-author("pizza@parseerror.com").
-export([parse/1, assemble/1, privmsg/2, default_port/0]).
-include_lib("irc.hrl").

parse(Str) ->
	% FIXME: we must preserve the rawtxt somehow...
	Split = string:tokens(Str, ":, "),
	mkmsg(Str, Split).

%% 

mkmsg(Raw, ["PING", Src]) ->
	#ircmsg{type="PING", src=Src, raw=Raw};

mkmsg(Raw, ["ERROR"=Src, Type, Dst | Txt]) ->
	#ircmsg{type=Src, src=Type, dst=Dst, txt=Txt, raw=Raw};

mkmsg(Raw, [Src, Type, Dst | Txt]) ->
	#ircmsg{type=Type, src=Src, dst=Dst, txt=Txt, raw=Raw}.

%%

assemble(#ircmsg{type="NICK"=Type, src=Src}) ->
	Type ++ " " ++ Src ++ "\r\n";

assemble(#ircmsg{type="USER"=Type, src=Src, txt=Txt}) ->
	Type ++ " " ++ Src ++ " " ++ join(" ", Txt) ++ "\r\n";

assemble(#ircmsg{type="PONG"=Type, rawtxt=Raw}) ->
	Type ++ " " ++ Raw ++ "\r\n";

assemble(#ircmsg{type="JOIN"=Type, rawtxt=Rawtxt}) ->
	Type ++ " :" ++ Rawtxt ++ "\r\n";

assemble(#ircmsg{type="PRIVMSG"=Type, dst=Dst, txt=Txt, rawtxt=""}) ->
	Type ++ " " ++ Dst ++ " :" ++ join(" ", Txt) ++ "\r\n";

assemble(#ircmsg{type="PRIVMSG"=Type, dst=Dst, txt=[], rawtxt=Rawtxt}) ->
	Type ++ " " ++ Dst ++ " :" ++ Rawtxt ++ "\r\n".

privmsg(Dst, [[_|_]]=Say) ->
	#ircmsg{type="PRIVMSG", dst=Dst, txt=Say};

privmsg(Dst, [_|_]=Say) ->
	#ircmsg{type="PRIVMSG", dst=Dst, rawtxt=Say}.

% for some stupid reason the "lists" module doesn't have a join()...
join(_, []) ->
	"";
join(Glue, [Head|Tail]) ->
	if [] == Tail ->
		Head;
	true ->
		Head ++ Glue ++ join(Glue, Tail)
	end.

default_port() ->
	6667.

test() ->
[
	{
		":valhall.no.eu.dal.net NOTICE AUTH :*** Looking up your hostname...",
		{ircmsg,"NOTICE","valhall.no.eu.dal.net","AUTH",
        ["***","Looking","up","your","hostname..."]}
	},
	{
		":valhall.no.eu.dal.net 001 mod_pizza :Welcome to the DALnet IRC Network mod_pizza!~mod_pizza@12.229.112.195",
		{ircmsg,"001","valhall.no.eu.dal.net","mod_pizza",
        ["Welcome","to","the","DALnet","IRC","Network",
         "mod_pizza!~mod_pizza@12.229.112.195"]}
	},
	{
		":valhall.no.eu.dal.net 002 mod_pizza :Your host is valhall.no.eu.dal.net, running version bahamut-1.8(06)",
		{ircmsg,"002","valhall.no.eu.dal.net","mod_pizza",
        ["Your","host","is","valhall.no.eu.dal.net","running",
         "version","bahamut-1.8(06)"]}
	},
	{
		":valhall.no.eu.dal.net 003 mod_pizza :This server was created Thu Mar 26 2009 at 16:23:13 CET",
		{ircmsg,"003","valhall.no.eu.dal.net","mod_pizza",
        ["This","server","was","created","Thu","Mar","26","2009",
         "at","16","23","13","CET"]}
	},
	{
		":valhall.no.eu.dal.net 004 mod_pizza valhall.no.eu.dal.net bahamut-1.8(06) aAbcdefFghiIjkKmnoOrRswxXy bceiIjklLmMnoOprRstv",
		{ircmsg,"004","valhall.no.eu.dal.net","mod_pizza",
        ["valhall.no.eu.dal.net","bahamut-1.8(06)",
         "aAbcdefFghiIjkKmnoOrRswxXy","bceiIjklLmMnoOprRst","v"]}
	},
	{
		":valhall.no.eu.dal.net 005 mod_pizza NETWORK=DALnet SAFELIST MAXBANS=200 MAXCHANNELS=20 CHANNELLEN=32 ...:20 PREFIX=(ov)@+ STATUSMSG=@+ :are available on this server",
		{ircmsg,"005","valhall.no.eu.dal.net","mod_pizza",
        ["NETWORK=DALnet","SAFELIST","MAXBANS=200","MAXCHANNELS=20",
         "CHANNELLEN=32","...","20","PREFIX=(ov)@","+",
         "STATUSMSG=@+","are","available","on","this","server"]}
	},
	{
		":valhall.no.eu.dal.net 251 mod_pizza :There are 121 users and 25948 invisible on 41 servers",
		{ircmsg,"251","valhall.no.eu.dal.net","mod_pizza",
        ["There","are","121","users","and","25948","invisible","on",
         "41","servers"]}
	},
	{
		":valhall.no.eu.dal.net 376 mod_pizza :End of /MOTD command.",
		{ircmsg,"376","valhall.no.eu.dal.net","mod_pizza",
        ["End","of","/MOTD","command."]}
	},
	{
		":mod_pizza MODE mod_pizza :+i",
		{ircmsg,"MODE","mod_pizza","mod_pizza",["+i"]}
	},
	{
		"PING :valhall.no.eu.dal.net",
		{ircmsg,"PING","valhall.no.eu.dal.net",[],[]}
	},
	{
		":spox!~spox@pool-98-108-144-112.ptldor.fios.verizon.net PRIVMSG #mod_spox :lol",
		{ircmsg,"PRIVMSG",
        "spox!~spox@pool-98-108-144-112.ptldor.fios.verizon.net",
        "#mod_spox",
        ["lol"]}
	},
	{
		":spox!~spox@pool-98-108-144-112.ptldor.fios.verizon.net PRIVMSG #mod_spox :mod_pizza, lol",
		{ircmsg,"PRIVMSG",
        "spox!~spox@pool-98-108-144-112.ptldor.fios.verizon.net",
        "#mod_spox",
        ["mod_pizza","lol"]}
	},
	{
		":jade.fl.us.dal.net 433 * mod_pizza :Nickname is already in use.",
		{ircmsg,"433","jade.fl.us.dal.net","*",
        ["mod_pizza","Nickname","is","already","in","use."],
        [],
        ":jade.fl.us.dal.net 433 * mod_pizza :Nickname is already in use."}
	},
	{
		"ERROR :Closing Link: 0.0.0.0 (Ping timeout)",
		{ircmsg,"ERROR","Closing","Link",
        ["0.0.0.0","(Ping","timeout)"],
        [],"ERROR :Closing Link: 0.0.0.0 (Ping timeout)"}
	}

].

