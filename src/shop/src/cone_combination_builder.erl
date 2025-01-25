-module(cone_combination_builder).
-export([cone_combinations/2, cone_combinations/1, most_popular_combinations/2]).

cone_combinations(Top_flavors, _Bottom_flavors) when not is_list(Top_flavors) -> fail;
cone_combinations(Top_flavors, Bottom_flavors) ->
    [{T, B} || T <- Top_flavors, B <- Bottom_flavors].

cone_combinations(Flavor_list) when not is_list(Flavor_list) -> fail;
cone_combinations(Flavor_list) ->
    [{T, B} || T <- Flavor_list, B <- Flavor_list].

most_popular_combinations(Count, _List) when Count < 0 -> fail;
most_popular_combinations(Count, List) when Count > length(List) -> fail;
most_popular_combinations(Count, List) ->
    {Keepers, _Rest} = lists:split(Count, List),
    Keepers.

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

cone_combinations_two_list_test_() ->
    Top_scoop_list = [vanilla, chocolate, cherry_ripple],
    Bottom_scoop_list = [lemon, butterscotch, licorice_ripple],
    Expected_combinations = [
        {vanilla, lemon}, {vanilla, butterscotch}, {vanilla, licorice_ripple},
        {chocolate, lemon}, {chocolate, butterscotch}, {chocolate, licorice_ripple},
        {cherry_ripple, lemon}, {cherry_ripple, butterscotch}, {cherry_ripple, licorice_ripple}
    ],
    [
        ?_assertEqual(Expected_combinations, cone_combinations(Top_scoop_list, Bottom_scoop_list)),
        ?_assertEqual([], cone_combinations(Top_scoop_list, [])),
        ?_assertEqual([], cone_combinations([], Bottom_scoop_list)),
        ?_assertEqual([], cone_combinations([], [])),
        ?_assertEqual(fail, cone_combinations(nil, [])),
        ?_assertEqual(fail, cone_combinations(nil, nil))
    ].

cone_combinations_one_list_test_() ->
    Flavor_list = [vanilla, chocolate, cherry_ripple],
    Expected_combinations = [
        {vanilla, vanilla}, {vanilla, chocolate}, {vanilla, cherry_ripple},
        {chocolate, vanilla}, {chocolate, chocolate}, {chocolate, cherry_ripple},
        {cherry_ripple, vanilla}, {cherry_ripple, chocolate}, {cherry_ripple, cherry_ripple}
    ],
    [
        ?_assertEqual(Expected_combinations, cone_combinations(Flavor_list)),
        ?_assertEqual([], cone_combinations([])),
        ?_assertEqual(fail, cone_combinations(nil))
    ].

most_popular_combinations_test_() ->
    All_combinations = [
        {vanilla, vanilla}, {vanilla, chocolate}, {vanilla, cherry_ripple},
        {chocolate, vanilla}, {chocolate, chocolate}, {chocolate, cherry_ripple},
        {cherry_ripple, vanilla}, {cherry_ripple, chocolate}, {cherry_ripple, cherry_ripple}
    ],
    Sorted_combinations_with_purchases =
        lists:sort(lists:zip([rand:uniform(100000) || _ <- lists:seq(1, length(All_combinations))], All_combinations)),
    {Top_combinations, _Rest} = lists:split(3, Sorted_combinations_with_purchases),
    [
        ?_assertEqual(Top_combinations, most_popular_combinations(3, Sorted_combinations_with_purchases)),
        ?_assertEqual([], most_popular_combinations(0, Sorted_combinations_with_purchases)),
        ?_assertEqual(fail, most_popular_combinations(-1, Sorted_combinations_with_purchases))
    ].
-endif.
