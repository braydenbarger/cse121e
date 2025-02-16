-module(friend_tracker).
-export([start/1, rpc/2, run/1]).
start(Initial_friends_list) ->
    spawn(?MODULE, run, [Initial_friends_list]).

%%
rpc(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        Response -> Response
    end.

%%

run(Friend_list) ->
    receive
        {From, {add, Friend}} ->
            New_friends = add_friend(Friend, Friend_list),
            From ! {ok, New_friends},
            run(New_friends);

        {From, {add, Friends}} when is_list(Friends) ->
            New_friends = add_friends(Friends, Friend_list),
            From ! {ok, New_friends},
            run(New_friends);

        {From, {has_friend, Friend}} ->
            If_has_friend = has_friend(Friend, Friend_list),
            From ! {If_has_friend},
            run(Friend_list);

        {From, {has_friends, Friends}} when is_list(Friends) ->
            If_has_friends = has_friends(Friends, Friend_list),
            From ! {If_has_friends},
            run(Friend_list);

        {From, {remove, Friend}} ->
            New_friends_after_removal = remove_friend(Friend, Friend_list),
            From ! {ok, New_friends_after_removal},
            run(New_friends_after_removal);

        {From, {remove, Friends}} when is_list(Friends) ->
            New_friends_after_removal_list = remove_friends(Friends, Friend_list),
            From ! {ok, New_friends_after_removal_list},
            run(New_friends_after_removal_list);

        {From, get} ->
            From ! Friend_list,
            run(Friend_list);

        {From, _} ->
            From ! {fail, unrecognized_message},
            run(Friend_list)
    end.

add_friend(Friend, Friend_list) ->
    [Friend | Friend_list].

add_friends(Friends, Friend_list) ->
    lists:append(Friends, Friend_list).

has_friend(Friend, Friend_list) ->
    lists:member(Friend, Friend_list).

has_friends([], _) -> 
    true;
has_friends([H | T], Friend_list) -> 
    has_friend(H, Friend_list) andalso has_friends(T, Friend_list).

remove_friend(Friend, Friend_list) ->
    lists:delete(Friend, Friend_list).

remove_friends([], Friend_list) -> 
    Friend_list;
remove_friends([H | T], Friend_list) ->
    remove_friends(T, lists:delete(H, Friend_list)).
