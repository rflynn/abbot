% ex: set ts=2 noet:
% $Id$
% irc command stuff

-module(cmd).
-author("pizza@parseerror.com").
-export([
	test/0,
	loop/0
	]).
-include_lib("../irc.hrl").
-import(irc).
-import(test).

test() ->
  true.

loop() ->
	receive
    { act, Pid, Irc, _, _, Nick, ["say", Chan | What] } ->
      say(Pid, Irc, Nick, Chan, What),
			loop();
    { act, Pid, Irc, _, _, Nick, ["act", Chan | What] } ->
      action(Pid, Irc, Nick, Chan, What),
			loop();
    { act, Pid, Irc, _, _, Nick, ["join" | Chans ] } ->
      join(Pid, Irc, Nick, Chans),
			loop();
    { act, Pid, Irc, _, _, Nick, ["part" | Chans ] } ->
      part(Pid, Irc, Nick, Chans),
			loop();
    { act, Pid, Irc, _, _, Nick, ["nick", NewNick] } ->
      nick(Pid, Irc, Nick, NewNick),
			loop();
    { act, Pid, Irc, _, _, Nick, ["plugins", "reload"] } ->
      plugins(Pid, Irc, Nick, "reload"),
			loop();
    { act, Pid, Irc, _, _, Nick, ["master", Master, Password] } ->
      master(Pid, Irc, Nick, Master, Password),
			loop();
    { act, Pid, Irc, _, _, Nick, ["quit" | Snarky] } ->
      quit(Pid, Irc, Nick, Snarky),
			loop();
		{ act, _, _, _, _, _, _ } -> % default
			loop();
		{ help, Pid, Dst, Nick } ->
			Pid ! {q,
        [ irc:resp(Dst, Nick, Nick ++ ": " ++ Que)
          || Que <-
          [
					  "[\"say\", Chan | What ] -> Say What in Chan",
					  "[\"act\", Chan | What ] -> /me What in Chan",
					  "[\"join\" | Chas      ] -> Join Chans",
					  "[\"part\" | Chans     ] -> Leave Chans)",
					  "[\"nick\", NewNick    ] -> Change nick",
					  "[\"quit\" | Snarky    ] -> Quit"
          ]
        ]
			},
			loop()
	end.

% if nick has sufficient perms Perm then run func Exec,
% else return Irc
byperm(Irc, Nick, _Perm, Exec) ->
	% TODO: implement real perm groups, etc.
	io:format("cmd byperm Nick=~p irc:master(Irc)=~p~n",
		[Nick, irc:master(Irc)]),
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

join(Pid, Irc, Nick, Chans) ->
	byperm(Irc, Nick, ["irc","join"],
		fun() ->
			Pid ! {q, [ #ircmsg{type="JOIN", rawtxt=Chan} || Chan <- Chans ]}
    end).

part(Pid, Irc, Nick, Chans) ->
	byperm(Irc, Nick, ["irc","part"],
		fun() ->
			Pid ! {q, [ #ircmsg{type="PART", rawtxt=Chan} || Chan <- Chans ]}
		end).

nick(Pid, Irc, Nick, NewNick) ->
	byperm(Irc, Nick, ["irc","nick"],
		fun() ->
			NewUser = (Irc#ircconn.user)#ircsrc{nick=NewNick},
			Irc2 = Irc#ircconn{user=NewUser},
			Pid ! {irc, Irc2},
			Pid ! {q, irc:nick(NewNick)}
		end).

master(Pid, Irc, Nick, Master, Password) ->
	if
		Password == Irc#ircconn.pass ->
			Pid ! {irc, Irc#ircconn{master=Master}},
			Pid ! {q, irc:privmsg(Nick, "Hello, master.")};
		true ->
			Pid ! {q, irc:privmsg(Nick, "Wrong password bucko!")}
	end.

plugins(Pid, Irc, Nick, "reload") ->
	byperm(Irc, Nick, ["core", "plugins","reload"],
		fun() ->
			Pid ! {plugins, reload}
		end).

quit(Pid, Irc, Nick, Snarky) ->
	byperm(Irc, Nick, ["irc","quit"],
		fun() ->
			Pid ! {q, #ircmsg{type="QUIT", rawtxt=util:j(Snarky)}}
		end).

