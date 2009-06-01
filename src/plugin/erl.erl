% ex: set ts=2 noet:
% $Id$
% allow us to eval erlang code

-module(erl).
-author("pizza@parseerror.com").
-export([
	test/0,
	loop/0,
  eval/1,
	filter/1,
	is_naughty/1,
	enumerate_naughty/1
	]).
-include_lib("irc.hrl").
-import(irc).
-import(test).

loop() ->
	receive
		{ act, Irc, #ircmsg{rawtxt="erl " ++ Code}, Dst, Nick, _ } ->
			act(Irc, Dst, Nick, Code);
		{ act, _, _, _, _, _, _ } ->
			nil;
		{ help, Pid, Dst, Nick } ->
			Pid ! {q,
				irc:resp(Dst, Nick, Nick ++ ": " ++
					"[\"erl\" | Code ] - evaluate erlang source code")
				}
	end.

% evaluate the input as erlang 
act(Irc, Dst, Nick, Code) ->
	io:format("Code=~p~n", [Code]),
	RespLines = exec(Code),
	Resps = % build an ircmsg for each line
		lists:map(fun(Line) -> irc:resp(Dst, Nick, Line) end,
			RespLines),
	bot:q(Irc, Resps).

% execute erlang source code and return [ "results" ]
% TODO: possibly make size of output configurable...
exec(Code) ->
	Res = erl:eval(Code), % eval
	Res2 = util:show(Res), % format
	Lines = util:lines(Res2),
	Lines2 = % limit number of lines
		if
			length(Lines) > 3 ->
				lists:sublist(Lines, 3) ++ "...";
			true ->
				Lines
			end,
	StrLines = [ util:show(L) || L <- Lines2 ],
	Out = string:join(StrLines, ""),
	TrimQuotes = string:substr(Out, 2, length(Out)-2),
	Unescaped = util:unescape(TrimQuotes),
	TrimQuotes2 = util:trim(Unescaped, $"),
 	Trimmed = % limit total length
		if
			length(TrimQuotes2) > 250 ->
				string:substr(TrimQuotes2, 1, 247) ++ "...";
			true -> TrimQuotes2
			end,
	util:lines(Trimmed).

-define(allowedAtoms,
	[
		nil,
		hd, tl, apply,
		lists,
			any,all,delete,dropwhile,filter,flatten,foldl,foldr,foreach,last,map,max,min,member,merge,nth,nthtail,partition,prefix,reverse,sort,split,splitwith,subtract,tail,unzip,usort,zip,zip3,zipwith,
		string,
			substr,
		io_lib,
			format
	]).

% HOW TO ALLOW A SAFE SUBSET:
% Restrict everything
% TODO: Restrict maximum exec time to something small
% DONE: rESTRICT ATOMS allowed to a WHITELIST
% TODO? Restruct integer values even, if we ever want to allow i.e. lists:seq

contains([], _) -> false;
contains([H|T], Item) ->
	if
		H =:= Item -> true;
		true -> contains(T, Item)
	end.

filter({atom, _N, Name}) ->
	not contains(?allowedAtoms, Name);
filter(_) -> false.

is_naughty(Tok) ->
	lists:any(
		fun(X) -> erl:filter(X) end, Tok).

enumerate_naughty(Tok) ->
	R1 = lists:map(fun(X) -> {X,erl:filter(X)} end, Tok),
	R2 = lists:filter(fun({_,Y}) -> Y =:= true end, R1),
	R3 = lists:map(
		fun({{atom,_,X},_}) -> atom_to_list(X) end, R2),
	"Not allowed: " ++ string:join(R3, ", ").

eval(Str) ->
  try
		io:format("Str=~p~n", [Str]),
    case erl_scan:string(Str) of
			{error, _, _} ->
				"syntax error";
			{ok, Tokens, _} ->
				Naughty = is_naughty(Tokens),
				if
					Naughty -> enumerate_naughty(Tokens);
					true ->
      			try
    					case erl_parse:parse_exprs(Tokens) of
      					{ok, [Form]} -> 
        					Bindings = erl_eval:new_bindings(),
          				case erl_eval:expr(Form, Bindings) of
            				{value, Fun, _} -> Fun;
            				_ -> "wtf"
          				end;
								{ok, Form} ->
        					Bindings = erl_eval:new_bindings(),
          				case erl_eval:expr(Form, Bindings) of
           					{value, Fun, _} -> Fun;
           					_ -> "wtf"
          				end;
        				{error, {_Errno, erl_parse, ["syntax error before: ", _Code]}} ->
        					"syntax error";
        				{error, {_Errno, erl_parse, [Msg, _Code]}} ->
        					Msg
      				end
       			catch
       				error:{unbound_var,Var} ->
         				"Unbound: " ++ atom_to_list(Var);
        			error:undef ->
         				[{Mod,Func,_}|_] = erlang:get_stacktrace(),
           			Name =
           				case Mod of
             				erl_eval -> 
             					atom_to_list(Func);
             				_ ->
                 			atom_to_list(Mod) ++ ":" ++
                 			atom_to_list(Func)
           				end,
            			"Undef fun: " ++ Name ++ "()";
         			error:illegal_expr -> "Illegal expr";
         			error:E -> atom_to_list(E)
       			end
				end
		end
  catch
    _ -> "Error"
  end.

test() ->
  test:unit(
    erl,
    [
      { eval,
        [
          { [ "" ], 					"syntax error" },
          { [ "." ], 					"syntax error" },
          { [ "nil." ], 			nil },
          { [ "0." ], 				0 },
          { [ "1." ], 				1 },
          { [ "a." ], 				"Not allowed: a" },
          { [ "_." ], 				"Unbound: _" },
          { [ "[]." ], 				[] },
          { [ "{}." ], 				{} },
          { [ "{}}." ], 			"syntax error" },
          { [ "\"\"." ], 			"" },
          { [ "+1." ], 				1 },
          { [ "1+1." ], 			2 },
          { [ "2+1." ], 			3 },
          { [ "a()." ], 			"Not allowed: a" },
          { [ "a(1,2,3)." ], 	"Not allowed: a" },
          { [ "a:b." ], 			"Not allowed: a, b" },
          { [ "a:b()." ], 		"Not allowed: a, b" },
          { [ "a:b:c." ], 		"Not allowed: a, b, c" },
          { [ "a:b:c()." ],		"Not allowed: a, b, c" },
					{ [ "\\" ], 				"syntax error" },
					{ [ "\\\\" ], 			"syntax error" },
					{ [ "\\\\\\" ], 		"syntax error" },
					{ [ "\"\"" ], 			"syntax error" },
          { [ "lists:map(fun(X)->X,[])." ], 						"syntax error" },
          { [ "lists:map(fun(_) -> nil end,[])." ], 		[] },
          { [ "lists:map(fun(X) -> X end,[])." ], 			[] },
          { [ "lists:map(fun(X)->X end,[1,2,3])." ], 		[1,2,3] },
          { [ "lists:map(fun(X)->X*2 end,[1,2,3])." ],	[2,4,6] },
					{ [ "Y=fun(N)->N+1 end. Y(1)." ], 						"syntax error" },
					{ [ "apply(fun(N)->N+1 end, [1])." ], 				2 },
					{ [ "[H|T]=[1,2,3], T." ], 										"function_clause" },
					{ [ " \":php echo \\\"abbot: ruby 1\\\\\"\".\r\n" ], "syntax error" }
        ]
      }
    ]
  ).

