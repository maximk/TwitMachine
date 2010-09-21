-module(lister).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([add/2]).
-export([clear/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(UserName, List) ->
	gen_server:call(?MODULE, {add,UserName,List}).

clear() ->
	gen_server:call(?MODULE, clear).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  tm_util:connect_db(),

  {A1, A2, A3} = erlang:now(),
  random:seed(A1, A2, A3),

  beat(1),
  {ok,[]}.

handle_call({add,UserName,List}, _From, ListSpecs) ->
  ListSpec = reset_timeline(UserName, List),
  {reply, ok, ListSpecs++[ListSpec]};

handle_call(clear, _From, _State) ->
  {reply, ok, []}.

handle_cast(beat, []) ->
  beat(5),
  {noreply, []};

handle_cast(beat, [ListSpec|ListSpecs]) ->
  ListSpec1 = do_list(ListSpec),
  beat(15),
  {noreply, ListSpecs++[ListSpec1]}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

reset_timeline(UserName, List) ->

	tm:slow_down_ip(),
	case twitter_api:list_timeline(UserName, List, 1) of
	{ok,[]} ->
		{UserName,List,0};
	{ok,Updates} ->
		Since = couchbeam_doc:get_value("id", hd(Updates)),
		{UserName,List,Since};
	X ->
		io:format("*** list_timeline fail:~n~p", [X]),
		receive after 10*1000 -> ok end,
		reset_timeline(UserName, List)
	end.

do_list({UserName,List,Since}=ListSpec) ->

	tm:slow_down_ip(),
	case twitter_api:list_timeline(UserName, List, 200, Since) of
	{ok,[]} ->
		ListSpec;

	{ok,Updates} ->
		NewSince = couchbeam_doc:get_value("id", hd(Updates)),
		classify_updates(UserName, List, Updates),
		{UserName,List,NewSince};

	X ->
		io:format("*** list_timeline fail:~n~p~n", [X]),
		ListSpec
	end.

%classify_updates(UserName, List, Updates) ->
%	io:format("######### ~p updates for ~s/~s:~n", [length(Updates),UserName,List]),
%	lists:foreach(fun(Upd) ->
%		UserDoc = couchbeam_doc:get_value("user", Upd),
%		Profile = classifier:profile(UserDoc),
%		ScreenName = binary_to_list(couchbeam_doc:get_value("screen_name", UserDoc)),
%		Description = binary_to_list(couchbeam_doc:get_value("description", UserDoc)),
%		Text = binary_to_list(couchbeam_doc:get_value("text", Upd)),
%		io:format("@~s ~p~nDescription: ~s~nStatus: ~s~n", [ScreenName,Profile,Description,Text])
%	end, Updates).

classify_updates(UserName, List, Updates) ->
	React = lists:filter(fun(Upd) ->
		classifier:react(Upd)
	end, Updates),
	lists:foreach(fun(Upd) ->
		UserDoc = couchbeam_doc:get_value("user", Upd),
		Profile = classifier:profile(UserDoc),
		ScreenName = binary_to_list(couchbeam_doc:get_value("screen_name", UserDoc)),
		case blurbs:gimme_text(Profile) of
		none ->
			io:format("??? No suitable template for ~p~n", [Profile]);
		Text ->
			Text1 = re:replace(Text, "@@", "@"++ScreenName, [{return,list}]),
			io:format("######## Recent reply from @~s (~s/~s)~n", [ScreenName,UserName,List]),
			tweet_me(Text1, ScreenName)
		end
	end, React).

tweet_me(Text, ScreenName) ->
	tm:trim_statuses(25),
	Tweets = tm:target_tweets(ScreenName),
	HasActive = lists:keymember(true, 2, Tweets),
	TooLong = length(Tweets) > 3,
	OnStopList = tm:on_stop_list(ScreenName),
	if not HasActive, not TooLong, not OnStopList ->
		User = select_random_user(),
		tm:status(User, Text, ScreenName);
	true ->
		io:format("### ~s skipped (tweeted to already)~n", [ScreenName])
	end.

select_random_user() ->
  	{A1, A2, A3} = erlang:now(),
    random:seed(A1, A2, A3),

    VR = couchbeam_db:query_view(tm_db, {"users","all"}, []),
    {_,_,_,Rows} = couchbeam_view:parse_view(VR),
    couchbeam_view:close_view(VR),
	hd(tm_util:shuffle([U || {U,_,_} <- Rows])).

beat(Secs) ->
	spawn(fun() ->
		receive
			after Secs*1000 ->
				gen_server:cast(?MODULE, beat)
		end
	end).

