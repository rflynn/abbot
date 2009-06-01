% ex: set ts=2 noet:
% $Id$
% irc command stuff

-module(irc).
-author("pizza@parseerror.com").
-export([
	test/0,
	loop/0
	]).
-include_lib("irc.hrl").
-import(irc).
-import(test).

test() ->
  true.

loop() ->
	receive
    { act, Pid, Irc, _, _, Nick, ["say", Chan | What] } ->
      say(Pid, Irc, Nick, Chan, What);
    { act, Pid, Irc, _, _, Nick, ["act", Chan | What] } ->
      action(Pid, Irc, Nick, Chan, What);
    { act, Pid, Irc, _, _, Nick, ["join", Chan ] } ->
      join(Pid, Irc, Nick, Chan);
    { act, Pid, Irc, _, _, Nick, ["part", Chan ] } ->
      part(Pid, Irc, Nick, Chan);
    { act, Pid, Irc, _, _, Nick, ["nick", NewNick] } ->
      nick(Pid, Irc, Nick, NewNick);
    { act, Pid, Irc, _, _, Nick, ["quit" | Snarky] } ->
      quit(Pid, Irc, Nick, Snarky);
		{ act, _, _, _, _, _, _ } ->
			nil;
		{ help, Pid, Dst, Nick } ->
			Pid ! {q,
        [ irc:resp(Dst, Nick, Nick ++ ": " ++ Que)
          || Que <-
          [
					  "[\"say\", Chan | What ] -> Say What in Chan",
					  "[\"act\", Chan | What ] -> /me What in Chan",
					  "[\"join\", Chan ] -> Join Chan",
					  "[\"part\", Chan ] -> Leave Chan",
					  "[\"nick\", NewNick ] -> Change nick",
					  "[\"quit\" | Snarky ] -> Quit"
          ]
        ]
			}
	end.

% if nick has sufficient perms Perm then run func Exec,
% else return Irc
byperm(Irc, Nick, _Perm, Exec) ->
	% TODO: implement real perm groups, etc.
	case Nick == irc:master(Irc) of
		true -> Exec();
		false -> Irc
		end.

% IRC-related command crap
say(Pid, Irc, Nick, Chan, What) ->
	byperm(Irc, Nick, ["irc","say"],
		fun() ->
      Pid ! {q, irc:privmsg(Chan, util:j(What))}
    end).

action(Pid, Irc, Nick, Chan, What) ->
	byperm(Irc, Nick, ["irc","action"],
		fun() ->
      Pid ! {q, irc:privmsg(Chan, irc:action(util:j(What)))}
    end).

join(Pid, Irc, Nick, Chan) ->
	byperm(Irc, Nick, ["irc","join"],
		fun() ->
			Pid ! {q, #ircmsg{type="JOIN", rawtxt=Chan}}
    end).

part(Pid, Irc, Nick, Chan) ->
	byperm(Irc, Nick, ["irc","part"],
		fun() ->
			Pid ! {q, #ircmsg{type="PART", rawtxt=Chan}}
		end).

nick(Pid, Irc, Nick, NewNick) ->
	byperm(Irc, Nick, ["irc","nick"],
		fun() ->
			NewUser = (Irc#ircconn.user)#ircsrc{nick=NewNick},
			Irc2 = Irc#ircconn{user=NewUser},
			Pid ! {irc, Irc2},
			Pid ! {q, irc:nick(NewNick)}
		end).

quit(Pid, Irc, Nick, Snarky) ->
	byperm(Irc, Nick, ["irc","quit"],
		fun() ->
			Pid ! {q, #ircmsg{type="QUIT", rawtxt=util:j(Snarky)}}
		end).

