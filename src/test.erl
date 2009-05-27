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

cnt(true) -> 1;
cnt(false) -> 0.

% test a function
% return bool whether testcase results match expected
testfunc(Mod, Test) ->
  {Func, Data} = Test,
	io:format("~p:~n", [Func]),
	Total = length(Data),
  Passed = each(Mod, Func, Data, Total, 0),
	io:format("\t~3w/~3w~n", [Passed, Total]),
	Passed == Total.

% test a single case for a function
each(_, _, [], _, Passed) ->
  Passed;
each(Mod, Func, [Expect|Rest], Total, Passed) ->
  {In, Out} = Expect,
	Add =
		case one(Mod, Func, In, Out) of
			true -> 1;
			false -> 0
			end,
  each(Mod, Func, Rest, Total, Passed + Add).

% mod:func(data) == expect?
one(Mod, Func, Input, Expect) ->
  Result = apply(Mod, Func, [Input]),
  case Expect == Result of
    false ->
			io:format("\t~p(~p) = ~p vs. ~p~n",
				[Func, Input, Expect, Result]);
		true -> nil
    end,
  Expect == Result.

