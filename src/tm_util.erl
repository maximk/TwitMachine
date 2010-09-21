-module(tm_util).
-export([connect_db/0]).
-export([now/0]).
-export([shuffle/1]).

-include_lib("couchbeam/include/couchbeam.hrl").

connect_db() ->
    case couchbeam_manager:get_db(tm_db) of
    not_found ->
        Params = #couchdb_params{host="localhost"},
        Conn = couchbeam_server:start_connection_link(Params),
        couchbeam_server:open_or_create_db(Conn, {tm_db, "twitmachine"});
    _ ->
        already_connected
    end.

now() ->
    {MegaSecs,Secs,_} = erlang:now(),
	MegaSecs * 1000000 + Secs.


shuffle([]) ->
    [];
shuffle(List) ->
    %% Determine the log n portion then randomize the list.
    randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
    randomize(List);
randomize(T, List) ->
    lists:foldl(fun(_E, Acc) ->
        randomize(Acc)
    end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
    D = lists:map(fun(A) ->
        {random:uniform(), A}
    end, List),
    {_, D1} = lists:unzip(lists:keysort(1, D)),
    D1.

%%EOF
