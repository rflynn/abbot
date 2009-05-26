% ex: set ts=2 noet:
% $Id$
% allow us to eval erlang code

-module(erl).
-author("pizza@parseerror.com").
-export([
  eval/1,
	test/0 ]).
-import(test).

eval(Str) ->
  try
    {ok, Tokens, _} = erl_scan:string(Str),
    case erl_parse:parse_exprs(Tokens) of
      {ok, [Form]} -> 
        Bindings = erl_eval:new_bindings(),
        try
          case erl_eval:expr(Form, Bindings) of
            {value, Fun, _} -> Fun;
            _ -> "wtf"
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
          end;
        {error, {_Errno, erl_parse, [_Msg, _Code]}} ->
        "Parse error";
        _ -> "root@box# Nah, just kidding"
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
          { "", "Parse error" },
          { ".", "Parse error" },
          { "nil.", nil },
          { "0.", 0 },
          { "1.", 1 },
          { "a.", a },
          { "_.", "Unbound: _" },
          { "[].", [] },
          { "{}.", {} },
          { "{}}.", "Parse error" },
          { "\"\".", "" },
          { "+1.", 1 },
          { "1+1.", 2 },
          { "2+1.", 3 },
          { "a().", "Undef fun: a()" },
          { "a(1,2,3).", "Undef fun: a()" },
          { "a:b.", "Illegal expr" },
          { "a:b().", "Undef fun: a:b()" },
          { "a:b:c.", "Parse error" },
          { "a:b:c().", "Parse error" },
          { "lists:map(fun(X)->X,[]).", "Parse error" },
          { "lists:map(fun(_) -> nil end,[]).", [] },
          { "lists:map(fun(X) -> X end,[]).", [] },
          { "lists:map(fun(X)->X end,[1,2,3]).", [1,2,3] },
          { "lists:map(fun(X)->X*2 end,[1,2,3]).", [2,4,6] }
        ]
      }
    ]
  ).


