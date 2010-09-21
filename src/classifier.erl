-module(classifier).
-export([profile/1]).
-export([react/1]).

react(UpdateDoc) ->
	InReply = couchbeam_doc:get_value("in_reply_to_screen_name", UpdateDoc),
	if InReply =/= null ->
		true;
	true ->
		Text = couchbeam_doc:get_value("text", UpdateDoc),
		Mentions = re:run(Text, "@[a-zA-Z_]") =/= nomatch,
		IsRT = re:run(Text, "via|RT") =/= nomatch,
		Mentions and not IsRT
	end.

%%{[{<<"follow_request_sent">>,false},
%%  {<<"profile_background_image_url">>,
%%   <<"http://a1.twimg.com/profile_background_images/87954856/astounded.jpg">>},
%%  {<<"profile_image_url">>,
%%   <<"http://a1.twimg.com/profile_images/1088695749/P1010617-1_normal.JPG">>},
%%  {<<"description">>,<<"Founder, paidContent.org & Contentnext Media">>},
%%  {<<"profile_sidebar_fill_color">>,<<"252429">>},
%%  {<<"url">>,<<"http://www.paidcontent.org">>},
%%  {<<"profile_background_tile">>,true},
%%  {<<"followers_count">>,5774},
%%  {<<"screen_name">>,<<"rafatali">>},
%%  {<<"lang">>,<<"en">>},
%%  {<<"created_at">>,<<"Sat Aug 02 22:52:01 +0000 2008">>},
%%  {<<"profile_sidebar_border_color">>,<<"181A1E">>},
%%  {<<"location">>,<<"New York City">>},
%%  {<<"listed_count">>,485},
%%  {<<"friends_count">>,301},
%%  {<<"statuses_count">>,4684},
%%  {<<"show_all_inline_media">>,false},
%%  {<<"following">>,true},
%%  {<<"favourites_count">>,2},
%%  {<<"profile_background_color">>,<<"1A1B1F">>},
%%  {<<"contributors_enabled">>,false},
%%  {<<"protected">>,false},
%%  {<<"geo_enabled">>,true},
%%  {<<"profile_text_color">>,<<"666666">>},
%%  {<<"name">>,<<"Rafat Ali">>},
%%  {<<"profile_use_background_image">>,true},
%%  {<<"time_zone">>,<<"Eastern Time (US & Canada)">>},
%%  {<<"id">>,15705567},
%%  {<<"verified">>,false},
%%  {<<"notifications">>,false},
%%  {<<"utc_offset">>,-18000},
%%  {<<"profile_link_color">>,<<"2FC2EF">>}]}

profile(UserDoc) -> %%{Type,Domain,Size,Autofollow}
	Description = binary_to_list(couchbeam_doc:get_value("description", UserDoc)),
	FriendsCount = couchbeam_doc:get_value("friends_count", UserDoc),
	FollowersCount = couchbeam_doc:get_value("followers_count", UserDoc),
	ListedCount = couchbeam_doc:get_value("listed_count", UserDoc),
	Type = profile_type(Description, FriendsCount, FollowersCount, ListedCount),
	Domain = profile_domain(Description, FriendsCount, FollowersCount, ListedCount),
	Size = profile_size(Description, FriendsCount, FollowersCount, ListedCount),
	Autofollow = profile_autofollow(Description, FriendsCount, FollowersCount, ListedCount),
	{Type,Domain,Size,Autofollow}.

profile_type(Description, _FriendsCount, _FollowersCount, _ListedCount) ->
	Keywords = [{author,["author","blogger","podcaster","journalist","column","editor","writer"]},
				{manager,["marketing","manager","strategist","consultant"]},
				{biz,["investor","entrepreneur","founder","seo"]},
				{clevel,["vice-president","vp","director"]}],
	LD = string:to_lower(Description),
	Types = lists:filter(fun({_Type,Ks}) ->
		lists:any(fun(K) -> string:str(LD, K) =/= 0 end, Ks)
	end, Keywords),
	case [T || {T,_} <- Types] of
	[] -> any;
	[X] -> X;
	_ -> multiple
	end.

profile_domain(Description, _FriendsCount, _FollowersCount, _ListedCount) ->
	Keywords = [{tech,["tech"]},
				{sports,["sport","football","baseball","golf"]},
				{politics,["politic"]}],
	LD = string:to_lower(Description),
	Domains = lists:filter(fun({_Domain,Ks}) ->
		lists:any(fun(K) -> string:str(LD, K) =/= 0 end, Ks)
	end, Keywords),
	case [T || {T,_} <- Domains] of
	[] -> any;
	[X] -> X;
	_ -> multiple
	end.

profile_size(_Description, _FriendsCount, FollowersCount, _ListedCount)
		when FollowersCount < 5000 ->
	0;
profile_size(_Description, _FriendsCount, FollowersCount, _ListedCount)
		when FollowersCount < 15000 ->
	1;
profile_size(_Description, _FriendsCount, FollowersCount, _ListedCount)
		when FollowersCount < 50000 ->
	2;
profile_size(_Description, _FriendsCount, _FollowersCount, _ListedCount) ->
	3.

profile_autofollow(_Description, FriendsCount, FollowersCount, _ListedCount) ->
	abs((FriendsCount-FollowersCount)/(FriendsCount+FollowersCount)) < 0.2.

%%EOF
