% ex: set ts=2 noet:
% $Id$

% USER <username> <hostname> <servername> <realname>

-record(ircsrc,
	{
		raw   = "",						% raw contents of src in received msg, i.e. "foo!~foo@bar"
		nick  = "",						% 
		user  = "",
		host	= ""
	}).

-record(ircconn,
	{
		host  = "",
    port  = 6667,
		key		= "",						% how we maintain relationship between ircconn and ircmsg
		user	= #ircsrc{},		% user we are on this server
		sock  = nil,					% socket
    server= "",
    real  = ""
	}).

-record(ircmsg,
	{
		host    = "",					% host received on
		user    = "",					% user we are on this connection
		type    = "",	  			% either a numeric irc code, PRIVMSG, NOTICE
		src     = #ircsrc{},  % who sent the msg; either no one, the server or a user
		dst     = "",	  			% where msg is headed; either AUTH, a nick (yourself) or a channel
		txt     = [],					% contents of what is sent, if anything
    rawtxt  = "",   			% unparsed, original contents of 'txt'
    raw     = ""    			% unparsed, original contents of entire message
	}).

