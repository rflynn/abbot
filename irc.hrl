% ex: set ts=2 noet:
% $Id$

-record(ircmsg,
	{
		type   = "",	% either a numeric irc code, PRIVMSG, NOTICE
		from   = "",	% who sent the msg; either no one, the server or a user
		target = "",	% where msg is headed; either AUTH, a nick (yourself) or a channel
		what   = ""		% contents of what is sent, if anything
	}).

