-module(team_builder).
-export([divide/2, lower_divide/2, remainder/2]).

divide(_, 0) -> fail;
divide(Dividend, Divisor) ->
    Dividend / Divisor.

lower_divide(_, 0) -> fail;
lower_divide(Dividend, Divisor) ->
    Dividend div Divisor.

remainder(_, 0) -> fail;
remainder(Dividend, Divisor) when not is_integer(Dividend) orelse not is_integer(Divisor) -> fail;
remainder(Dividend, Divisor) ->
    Dividend rem Divisor.

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

divide_test_() ->
    [?_assertEqual(2.0, divide(10, 5)),
     ?_assertEqual(fail, divide(3, 0)),
     ?_assertEqual(0.75, divide(3, 4)),
     ?_assert(3.5714285713 =< divide(1.25, 0.35)),
     ?_assert(3.5714285715 >= divide(1.25, 0.35)),
     ?_assert((-0.6666666 >= divide(2, -3)) and (-0.7 =< divide(2, -3))),
     ?_assert((-0.6666666 >= divide(-2, 3)) and (-0.7 =< divide(-2, 3))),
     ?_assert((0.6666666 =< divide(-2, -3)) and (0.7 >= divide(-2, -3)))].

lower_divide_test_() ->
    [?_assertEqual(2, lower_divide(10, 5)),
     ?_assertEqual(fail, lower_divide(10, 0)),
     ?_assertEqual(-2, lower_divide(-10, 5)),
     ?_assertEqual(-2, lower_divide(10, -5)),
     ?_assertEqual(2, lower_divide(-10, -5)),
     ?_assertEqual(0, lower_divide(5, 10)),
     ?_assertEqual(0, lower_divide(2, 3)),
     ?_assertEqual(0, lower_divide(-2, 10)),
     ?_assertEqual(0, lower_divide(2, -10))].

remainder_test_() ->
    [?_assertEqual(0, remainder(10, 5)),
     ?_assertEqual(fail, remainder(5, 0)),
     ?_assertEqual(fail, remainder(5.0, 2.0)),
     ?_assertEqual(5, remainder(5, 10)),
     ?_assertEqual(-5, remainder(-5, 10)),
     ?_assertEqual(-5, remainder(-5, -10)),
     ?_assertEqual(0, remainder(0, 7))].

-endif.
