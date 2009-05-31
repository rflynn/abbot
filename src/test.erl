% ex: set ts=2 noet:
% $Id$

-module(test).
-author("pizza@parseerror.com").
-export([unit/2]).

% top-level unit test, example from module foo:
%   test:unit(foo, [{bar, test_bar()}])
unit(Mod, Tests) ->
  Passed = unit_(Mod, Tests, length(Tests), 0),
  case Passed of
    true -> true;
    false ->
      io:format("Test failed.~n"),
      false
    end.

% run all unit tests, print summary, and return true/false
unit_(_, [], Cnt, Passed) when Passed =< Cnt ->
  io:format("~w/~w passed (~6.2f%)~n",
    [Passed, Cnt, Passed / Cnt * 100]),
  Passed == Cnt;
unit_(Mod, [Test|Rest], Cnt, Passed) when Passed =< Cnt ->
  unit_(Mod, Rest, Cnt, Passed + cnt(testfunc(Mod, Test))).

% test a function
% return bool whether testcase results match expected
testfunc(Mod, Test) ->
	{Func, Data, Cmp} =
		case Test of % set default comparator
  		{_, _, _} -> Test;
  		{Func2, Data2} -> {Func2, Data2,
				fun(Expect,Result) -> Expect =:= Result end}
		end,
	io:format("~p:~n", [Func]),
	Total = length(Data),
  Passed = each(Mod, Func, Data, Total, 0, Cmp),
	io:format("\t~3w/~3w~n", [Passed, Total]),
	Passed == Total.

% test a single case for a function
each(_, _, [], _, Passed, _) ->
  Passed;
each(Mod, Func, [Expect|Rest], Total, Passed, Cmp) ->
  {In, Out} = Expect,
	Add = cnt(one(Mod, Func, In, Out, Cmp)),
  each(Mod, Func, Rest, Total, Passed + Add, Cmp).

% mod:func(data) == expect?
one(Mod, Func, Input, Expect, Cmp) when is_list(Input) ->
  Result = apply(Mod, Func, Input),
	Match = Cmp(Expect, Result),
  case Match of
    false ->
			io:format("\t~p(~p) = Expected(~p) Result(~p)~n",
				[Func, Input, Expect, Result]);
		true -> nil
    end,
  Match.

cnt(true) -> 1;
cnt(false) -> 0.

