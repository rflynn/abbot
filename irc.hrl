% ex: set ts=2 noet:
% $Id$

-record(ircmsg,
	{
		type    = "",	  % either a numeric irc code, PRIVMSG, NOTICE
		src     = "",	  % who sent the msg; either no one, the server or a user
		dst     = "",	  % where msg is headed; either AUTH, a nick (yourself) or a channel
		txt     = [],		% contents of what is sent, if anything
    rawtxt  = "",   % unparsed, original contents of 'txt'
    raw     = ""    % unparsed, original contents of entire message
	}).

