% ex: set ts=2 noet:
% $Id$
% allow us to eval erlang code

-module(erl).
-author("pizza@parseerror.com").
-export([
  eval/1,
	filter/1,
	is_naughty/1,
	enumerate_naughty/1,
	test/0 ]).
-import(test).
-define(allowedAtoms,
	[
		nil,
		hd, tl, apply,
		lists,
			any,all,filter,foldl,foldr,foreach,last,map,max,min,nth,tail,
		string,
			substr,
		io_lib,
			format
	]).

% HOW TO ALLOW A SAFE SUBSET:
% Restrict everything
% TODO: Restrict maximum exec time to something small
% DONE: Restrict atoms allowed to a whitelist
% TODO? Restruct integer values even

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
    {ok, Tokens, _} = erl_scan:string(Str),
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
        		{error, {_Errno, erl_parse, [_Msg, _Code]}} ->
        			_Msg
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
  catch
    _ -> "Error"
  end.

test() ->
  test:unit(
    erl,
    [
      { eval,
        [
          { "", "syntax error before: " },
          { ".", "syntax error before: " },
          { "nil.", nil },
          { "0.", 0 },
          { "1.", 1 },
          { "a.", "Not allowed: a" },
          { "_.", "Unbound: _" },
          { "[].", [] },
          { "{}.", {} },
          { "{}}.", "syntax error before: " },
          { "\"\".", "" },
          { "+1.", 1 },
          { "1+1.", 2 },
          { "2+1.", 3 },
          { "a().", "Not allowed: a" },
          { "a(1,2,3).", "Not allowed: a" },
          { "a:b.", "Not allowed: a, b" },
          { "a:b().", "Not allowed: a, b" },
          { "a:b:c.", "Not allowed: a, b, c" },
          { "a:b:c().", "Not allowed: a, b, c" },
          { "lists:map(fun(X)->X,[]).", "syntax error before: " },
          { "lists:map(fun(_) -> nil end,[]).", [] },
          { "lists:map(fun(X) -> X end,[]).", [] },
          { "lists:map(fun(X)->X end,[1,2,3]).", [1,2,3] },
          { "lists:map(fun(X)->X*2 end,[1,2,3]).", [2,4,6] },
					{ "Y=fun(N)->N+1 end. Y(1).", "syntax error before: " },
					{ "apply(fun(N)->N+1 end, [1]).", 2 },
					{ "[H|T]=[1,2,3], T.", "function_clause" }
        ]
      }
    ]
  ).


