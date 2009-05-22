% ex: set ts=2 noet:
% $Id$

-module(irc).
-author("pizza@parseerror.com").
-export([parse/1]).
-include_lib("irc.hrl").

parse(Str) ->
	Split = string:tokens(Str, ":, "),
	irc_mkmsg(Split).

irc_mkmsg([From, Type, Target | What]) ->
	#ircmsg{type=Type, from=From, target=Target, what=What};

irc_mkmsg(["PING", From]) ->
	#ircmsg{type="PING", from=From}.

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
	}
].

