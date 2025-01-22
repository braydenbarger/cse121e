-module(team_builder).
-export([add/2,divide/2,lower_divide/2,remainder/2]).

add(A,B) ->
    A+B.

divide(Dividend,Divisor) ->
    Dividend/Divisor.

lower_divide(Dividend,Divisor) ->
    Dividend div Divisor.

remainder(Dividend,Divisor) ->
    Dividend rem Divisor.

difference(Minuend,Subtrahend) ->
    Minuend - Subtrahend.

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

difference_test_() ->
    [?_assertEqual(3,difference(9,6)),
     ?_assertEqual(0,difference(0,0)),
     ?_assert((0.2872 >= difference(0.756,0.4689)) and (0.2870  =< difference(0.756,0.4689))),
     ?_assertEqual(-12_345_678_894_321_002_220,difference(3_456_782_835,12_345_678_897_777_785_055)),
     ?_assertEqual(17,difference(10,-7)),
     ?_assertEqual(-17,difference(-10,7)),
     ?_assertEqual(-3,difference(-10,-7))
    ].

add_test_() ->
    [?_assertEqual(15,add(9,6)),
     ?_assertEqual(0,add(0,0)),
     ?_assert((1.23445 >= add(0.756,0.4689)) and (add(0.756,0.4689) =< 1.3)),
     ?_assertEqual(12_345_678_901_234_567_890,add(3_456_782_835,12_345_678_897_777_785_055)),
     ?_assertEqual(-17,add(-10,-7)),
     ?_assertEqual(-17,add(20,-37))
    ].

divide_test_() ->
    [?_assertEqual(2.0,divide(10,5)),
     ?_assertEqual(0.75,divide(3,4)),
     ?_assert(3.5714285713 =< divide(1.25,0.35)),
     ?_assert(3.5714285715 >= divide(1.25,0.35)),
     ?_assert((-0.6666666 >= divide(2,-3)) and (-0.7 =< divide(2,-3))),
     ?_assert((-0.6666666 >= divide(-2,3)) and (-0.7 =< divide(2,-3))),
     ?_assert((0.6666666 =< divide(-2,-3)) and (0.7 >= divide(2,-3)))
    ].

lower_divide_test_() ->
    [?_assertEqual(2,lower_divide(10,5)),
     ?_assertEqual(-2,lower_divide(-10,5)),
     ?_assertEqual(-2,lower_divide(10,-5)),
     ?_assertEqual(2,lower_divide(-10,-5)),
     ?_assertEqual(0,lower_divide(5,10)),
     ?_assertEqual(0,lower_divide(2,3)),
     ?_assertEqual(0,lower_divide(-2,10)),
     ?_assertEqual(0,lower_divide(2,-10))
    ].

remainder_test_() ->
    [?_assertEqual(0,remainder(10,5)),
     ?_assertEqual(5,remainder(5,10)),
     ?_assertEqual(-5,remainder(-5,10)),
     ?_assertEqual(-5,remainder(-5,-10)),
     ?_assertEqual(0,remainder(0,7))
    ].

random_list_test_() ->
    Random_numbers = [rand:uniform(10) || _ <- lists:seq(1,10)],
    [?_assert(is_list(Random_numbers)),
     ?_assertEqual(10,length(Random_numbers)),
     ?_assertEqual([],lists:filter(fun(N)-> N > 10 end,Random_numbers))
    ].

person_tuple_list_test_() ->
    People = [{sue,female,1.6},{grace,female,1.5},{joe,male,0.5},{alma,female,1.0007},{alma,male,1.75},
              {gunhilda,female,0.701},{jose,male,1.72},{sven,male,1.605}],
    [?_assert(is_list(People)),
     ?_assert(length(People) >= 8),
     ?_assert(lists:all(fun(Person)-> is_tuple(Person) end,People)),
     ?_assert(lists:all(fun({Name,Gender,Height})-> is_atom(Name) and is_atom(Gender) and is_float(Height) end,People))
    ].

gender_list_test_() ->
    Genders = [male,female,other],
    [?_assert(lists:member(male,Genders)),
     ?_assert(lists:member(female,Genders)),
     ?_assert(lists:member(other,Genders)),
     ?_assertNot(lists:member(none,Genders))
    ].
-endif.
