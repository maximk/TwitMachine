-module(tm).
-export([slow_down/1,slow_down_ip/0]).
-export([drop_tweeps/0]).
-export([grab_list/3]).
-export([update_friends/1]).
-export([create_list/3]).
-export([status/3]).
-export([forget_status/2]).
-export([target_tweets/1]).
-export([trim_statuses/1]).
-export([on_stop_list/1]).

slow_down(User) ->
	slow_down_ip(),
	slow_down_user(User).

drop_tweeps() ->
	drop_tweeps(0).

drop_tweeps(N) ->
	VR = couchbeam_db:query_view(tm_db, {"tweeps","all"}, [{"limit",10},{"include_docs",true}]),
	{_,_,_,Rows} = couchbeam_view:parse_view(VR),
	couchbeam_view:close_view(VR),
	case length(Rows) of
	0 ->
		io:format("~p tweep(s) dropped~n", [N]);
	K ->
		Docs = [Doc || {_,_,_,Doc} <- Rows],
		couchbeam_db:delete_docs(tm_db, Docs),
		drop_tweeps(N+K)
	end.

slow_down_ip() ->
	case twitter_api:rate_limit_status(none) of
	{ok,{ResetTime,0}} ->
		Secs = ResetTime - tm_util:now(),
		io:format("Slow down for ~p sec(s)~n", [Secs]),
		sleep(Secs);
	{ok,_} ->
		ok;
	_ ->
		io:format("*** RETRY rate_limit_status~n", []),
		sleep(10),
		slow_down_ip()
	end.

slow_down_user(User) ->
	Auth = user_auth(User),
	case twitter_api:rate_limit_status(Auth) of
	{ok,{ResetTime,0}} ->
		Secs = ResetTime - tm_util:now(),
		io:format("Slow down ~p for ~p sec(s)~n", [User,Secs]),
		sleep(Secs);
	{ok,_} ->
		ok;
	_ ->
		io:format("*** RETRY rate_limit_status(~p)~n", [User]),
		sleep(10),
		slow_down_ip()
	end.

grab_list(User, Owner, List) ->
	grab_list(User, Owner, List, 0, -1).

grab_list(_User, _Owner, _List, N, 0) ->
	io:format("~p tweep(s) grabbed~n", [N]);
grab_list(User, Owner, List, N, Cursor) ->
	slow_down(User),
	Auth = user_auth(User),
	case twitter_api:get_list_members(Auth, Owner, List, Cursor) of
	{ok,{Users,NextCursor}} ->
		lists:foreach(fun(Doc) ->
			ScreenName = couchbeam_doc:get_value("screen_name", Doc),
			Doc1 = couchbeam_doc:set_value("_id", ScreenName, Doc),
			Doc2 = couchbeam_doc:set_value("type", <<"tweep">>, Doc1),
			couchbeam_db:save_doc(tm_db, Doc2)
		end, Users),
		grab_list(User, Owner, List, N+length(Users), NextCursor);
	X ->
		io:format("*** RETRY get_list_members:~n~p~n", [X]),
        sleep(10),
		grab_list(User, Owner, List, N, Cursor)
	end.

update_friends(User) ->
	VR = couchbeam_db:query_view(tm_db, {"authorities","no_friends"}, []),
	{_,_,_,Rows} = couchbeam_view:parse_view(VR),
	couchbeam_view:close_view(VR),
	Names = [Name || {Name,_,_} <- Rows],
	if Names == [] ->
		io:format("All authorities' friends are known~n", []);
	true ->
		update_friends(User, Names, 0)
	end.

update_friends(_User, [], N) ->
	io:format("Friends updates for ~p autorities~n", [N]);
update_friends(User, [Name|Names], N) ->
	slow_down(User),
	Auth = user_auth(User),
	case twitter_api:friend_ids(Auth, binary_to_list(Name)) of
	{ok,Friends} ->
		Doc = couchbeam_db:open_doc(tm_db, Name),
		Doc1 = couchbeam_doc:set_value("friends", Friends, Doc),
		couchbeam_db:save_doc(tm_db, Doc1),
		update_friends(User, Names, N+1);
	X ->
		io:format("*** RETRY friends_ids:~n~p~n", [X]),
        sleep(10),
		update_friends(User, [Name|Names], N)
	end.

create_list(User, List, MemberFile) ->
	{ok,Members} = file:consult(MemberFile),
	slow_down(User),
	Auth = user_auth(User),
	case twitter_api:make_list(Auth, binary_to_list(User), List) of
	ok ->
		lists:foreach(fun(Id) ->
			twitter_api:list_add(Auth, binary_to_list(User), List, Id)
		end, Members),
		io:format("~p members added to ~p~n", [length(Members),List]);
	X ->
		io:format("*** RETRY make_list:~n~p~n", [X]),
        sleep(10),
		create_list(User, List, MemberFile)
	end.

status(User, Text, Target) ->
	slow_down(User),
	Auth = user_auth(User),
	case twitter_api:status_update(Auth, Text) of
	{ok,Id} ->
		DocId = list_to_binary(integer_to_list(Id)),
		Doc = {[{<<"created">>,tm_util:now()},
			    {<<"type">>,<<"tweet">>},
				{<<"user">>,User},
				{<<"target">>,list_to_binary(Target)},
				{<<"text">>,list_to_binary(Text)},
				{<<"active">>,true},
				{<<"_id">>,DocId}]},
		couchbeam_db:save_doc(tm_db, Doc),
		io:format("#### ~p (~p) ~s~n", [User,Id,Text]);
	X ->
		io:format("*** NORETRY status_update:~n~p~n", [X])
	end.

forget_status(User, Id) ->
    slow_down(User),
    Auth = user_auth(User),
    case twitter_api:delete_status(Auth, Id) of
    ok ->
		DocId = list_to_binary(integer_to_list(Id)),
		Doc = couchbeam_db:open_doc(tm_db, DocId),
		Doc1 = couchbeam_doc:set_value("active", false, Doc),
		couchbeam_db:save_doc(tm_db, Doc1),
        io:format("#### ~p status ~p forgotten~n", [User,Id]);
    X ->
        io:format("*** NORETRY status_update:~n~p~n", [X])
    end.

target_tweets(Target) ->
	Key = list_to_binary(Target),
	VR = couchbeam_db:query_view(tm_db, {"tweets","by_target"}, [{"key",Key}]),
	{_,_,_,Rows} = couchbeam_view:parse_view(VR),
	couchbeam_view:close_view(VR),
	[{bin_to_int(Id),Active} || {Id,_,Active} <- Rows].

trim_statuses(N) ->
	VR = couchbeam_db:query_view(tm_db, {"tweets","active"}, []),
	{_,_,_,Rows} = couchbeam_view:parse_view(VR),
	couchbeam_view:close_view(VR),

	if length(Rows) > N ->
		Extra = length(Rows) - N,
		{Expired,_} = lists:split(Extra, [{bin_to_int(Id),User} || {Id,_,User} <- Rows]),
		lists:foreach(fun({Id,User}) ->
			forget_status(User, Id)
		end, Expired),
		io:format("## ~w tweet(s) trimmed~n", [length(Expired)]);
	true ->
		ok
	end.

on_stop_list(ScreenName) ->
	Name = list_to_binary(ScreenName),
	StopListDoc = couchbeam_db:open_doc(tm_db, <<"stoplist">>),
	Stopped = couchbeam_doc:get_value("tweeps", StopListDoc),
	lists:member(Name, Stopped).

bin_to_int(Bin) ->
	list_to_integer(binary_to_list(Bin)).

user_auth(User) ->
	UserDoc = couchbeam_db:open_doc(tm_db, User),
	AuthDoc = couchbeam_doc:get_value("twitter_auth", UserDoc),
	Token = binary_to_list(couchbeam_doc:get_value("access_token", AuthDoc)),
	TokenSecret = binary_to_list(couchbeam_doc:get_value("access_token_secret", AuthDoc)),
	{Token,TokenSecret}.

sleep(N) ->
	receive after N*1000 -> ok end.

%%EOF
