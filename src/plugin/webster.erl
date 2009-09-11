% ex: set ts=2 et:

% dictionary plugin for abbot

-module(webster).
-author("pizza@parseerror.com").
-export([
    test/0,
    loop/0
  ]).

-include_lib("../irc.hrl").
-import(irc).
-import(ircutil).
-import(test).

-define(dict_url,   "http://services.aonaware.com/DictService/Default.aspx" ++
                    "?action=define&dict=*&query=").
-define(timeout,    5000).

test() ->
  true.

loop() ->
  receive
    {act, Pid, _, Msg, Dst, Nick, ["webster" | Term]} ->
      lookup(Pid, Msg, Dst, Nick, Term),
      loop();
    {act, Pid, _, Msg, Dst, Nick, ["define" | Term]} ->
      lookup(Pid, Msg, Dst, Nick, Term),
      loop();
    {act, _, _, _, _, _, _} ->
      loop();
    {help, Pid, Dst, Nick} ->
      Pid ! {q,
        [ irc:resp(Dst, Nick, Nick ++ ": " ++ Txt) || Txt <-
          [ "[\"webster\" | Term] -> look up Term in dictionary",
            "[\"define\"  | Term] -> alias for webster" ]]
      },
      loop()
  end.

lookup(_, _, _, _, []) -> nil;
lookup(Pid, Msg, Dst, Nick, Term) ->
  Term2 = util:trim(util:j(Term)),
  Term3 = util:trim(Term2, $"),
  Term4 = util:trim(Term3),
  Url = ?dict_url ++ cgi:url_encode(Term4),
  {Code, Content} = content(Url),
  case Code of
    error ->
      Pid ! {q, irc:resp(Dst, Nick,
        lists:flatten(io_lib:format("define -> ~s", [Code])))};
    _ -> ok(Pid, Msg, Dst, Nick, Content)
  end.

content(Url) ->
  case http:request(get,{Url,[]},[{timeout, ?timeout}],[]) of
    {ok, {{_Http, Code, _Msg}, _Header, Content}} -> {Code, Content };
    {error, Why} -> {error, Why}
    end.

% url fetch succeeded, display items
ok(Pid, Msg, Dst, Nick, Content) ->
  Def = def(Content),
  if
    "" == Def -> nil;
    true ->
      Pid ! {pipe, Msg, irc:resp(Dst, Nick, Def)}
  end.

def(Content) ->
  Doc = mochiweb_html:parse(Content),
  F = fun(_Ctx, [String]) ->
    proplists:get_value(String, [{<<"link">>,1},{<<"myUrl">>,2}], 0) end,
  MyFuns = [{my_fun, F, [string]}],
  Raw = mochiweb_xpath:execute("//span[@id='lblDefinition']//text()", Doc, MyFuns),
  io:format("Raw=~p~n", [Raw]),
  Bin = lists:foldl( % get the first entry that is at least 200 bytes
    fun(B,Longest) ->
      L = 
        if
          is_binary(B) -> binary_to_list(B);
          true -> B
        end,
      if
        (length(Longest) < 200) and (length(L) > length(Longest)) -> L;
        true -> Longest
      end
    end, [], Raw),
  io:format("Bin=~p~n",[Bin]),
  Txt = util:j(ircutil:stripjunk([Bin])),
  io:format("Txt2=~p~n",[Txt]),
  Txt2 = util:j(string:tokens(Txt, " ,\r\n\t")),
  ircutil:dotdotdot(Txt2,400).

