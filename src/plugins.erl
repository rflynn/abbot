% ex: set ts=2 noet:
% $Id$
% manage bot plugins

-module(plugins).
-author("pizza@parseerror.com").
-export(
	[
		test/0,
    load/0,
    run/1,
    restart/2
	]).
-define(dir,	 "plugin").

test() ->
  true.

% find, load, test and return all available plugins
load() ->
	case file:list_dir(?dir) of
		{ok, Files} ->
			Ext = code:objfile_extension(), % ".beam"
			Files2 = lists:filter( % all .beam files
				fun(F) -> string:str(F, Ext) /= 0 end, Files),
			Names = [ % parse prefix
				string:substr(Filename, 1,
					string:str(Filename, Ext) - 1)
						|| Filename <- Files2 ],
			io:format("Plugins=~p~n", [Names]),
			[ plugins:run(Name) || Name <- Names ];
		_ ->
			io:format("'~s' directory not found. exiting.~n", [?dir]),
			exit(whereareyou)
		end.

% given a plugin module name as a string,
% load, unit test, spawn process and link plugin
% and return record
run(Name) ->
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
restart(Plugins, DeadPid) ->
	lists:map(
		fun({Name, _Atom, Pid}=P) ->
			if
				Pid == DeadPid ->
					io:format("Pid ~p was the ~s plugin, restarting...~n",
						[DeadPid, Name]),
					plugins:run(Name);
				true -> P
			end
		end,
		Plugins).

