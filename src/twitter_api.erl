-module(twitter_api).
-export([rate_limit_status/1]).
-export([get_list_members/3,get_list_members/4]).
-export([friend_ids/2]).
-export([make_list/3]).
-export([list_add/4]).
-export([list_timeline/3,list_timeline/4]).
-export([status_update/2]).
-export([delete_status/2]).

rate_limit_status(Auth) ->
	URL = "http://api.twitter.com/1/account/rate_limit_status.json",
	case rate_limit_status_request(URL, Auth) of 
	{ok, Response={{_, Status, _}, _Headers, Body}} ->
		if Status == 200 ->
			{ok,fields(["reset_time_in_seconds","remaining_hits"], Body)};
		true ->
			Response
		end;
	X ->
		X
	end.

rate_limit_status_request(URL, none) ->
	httpc:request(get, {URL, []}, [], []);
rate_limit_status_request(URL, {Token,TokenSecret}) ->
	oauth_get(header, URL, [], tm_conf:consumer(), Token, TokenSecret).

get_list_members({Token,TokenSecret}, Owner, List) ->
	get_list_members({Token,TokenSecret}, Owner, List, -1).

get_list_members({Token,TokenSecret}, Owner, List, Cursor) ->
	URL = "http://api.twitter.com/1/"++Owner++"/"++List++"/members.json",
	Params = [{"cursor",integer_to_list(Cursor)}],
	case oauth_get(header, URL, Params, tm_conf:consumer(), Token, TokenSecret) of
	{ok, Response={{_, Status, _}, _Headers, Body}} ->
		if Status == 200 ->
			{ok,fields(["users","next_cursor"], Body)};
		true ->
			Response
		end;
	X ->
		X
	end.

friend_ids({Token,TokenSecret}, Name) ->
	URL = "http://api.twitter.com/1/friends/ids.json",
	Params = [{"screen_name",Name}],
    case oauth_get(header, URL, Params, tm_conf:consumer(), Token, TokenSecret) of
    {ok, Response={{_, Status, _}, _Headers, Body}} ->
        if Status == 200 ->
			{ok,couchbeam:json_decode(Body)};
        true ->
            Response
        end;
    X ->
        X
    end.

make_list({Token,TokenSecret}, UserName, List) ->
	URL = "http://api.twitter.com/1/"++UserName++"/lists.json",
	Params = [{"name",List}],
	case oauth_post(header, URL, Params, tm_conf:consumer(), Token, TokenSecret) of
    {ok, Response={{_, Status, _}, _Headers, _Body}} ->
        if Status == 200 ->
            ok;
        true ->
            Response
        end;
    X ->
        X
    end.

list_add({Token,TokenSecret}, UserName, List, Id) ->
	URL = "http://api.twitter.com/1/"++UserName++"/"++List++"/members.json",
    Params = [{"id",integer_to_list(Id)}],
    case oauth_post(header, URL, Params, tm_conf:consumer(), Token, TokenSecret) of
    {ok, Response={{_, Status, _}, _Headers, _Body}} ->
        if Status == 200 ->
            ok;
        true ->
            Response
        end;
    X ->
        X
    end.

list_timeline(UserName, List, PerPage) ->
	list_timeline(UserName, List, PerPage, 0).

list_timeline(UserName, List, PerPage, Since) ->
	URL = "http://api.twitter.com/1/"++UserName++"/lists/"++List++"/statuses.json",
	Params = if Since == 0 -> []; true -> [{"since_id",integer_to_list(Since)}] end
		++ [{"per_page",integer_to_list(PerPage)}],
	URLParams = oauth:uri(URL, Params),
	case httpc:request(get, {URLParams,[]}, [], []) of
    {ok, Response={{_, Status, _}, _Headers, Body}} ->
        if Status == 200 ->
            {ok,couchbeam:json_decode(Body)};
        true ->
            Response
        end;
    X ->
        X
    end.

status_update({Token,TokenSecret}, Text) ->
	URL = "http://api.twitter.com/1/statuses/update.json",
	Params = [{"status",Text}],
	case oauth_post(header, URL, Params, tm_conf:consumer(), Token, TokenSecret) of
	{ok, Response={{_, Status, _}, _Headers, Body}} ->
        if Status == 200 ->
			Doc = couchbeam:json_decode(Body),
			Id = couchbeam_doc:get_value("id", Doc),
			{ok,Id};
        true ->
            Response
        end;
    X ->
        X
    end.

delete_status({Token,TokenSecret}, Id) ->
	URL = "http://api.twitter.com/1/statuses/destroy/"++integer_to_list(Id)++".json",
	case oauth_post(header, URL, [], tm_conf:consumer(), Token, TokenSecret) of
    {ok, Response={{_, Status, _}, _Headers, _Body}} ->
        if Status == 200 ->
            ok;
        true ->
            Response
        end;
    X ->
        X
    end.

fields(Flds, Body) ->
	Doc = couchbeam:json_decode(Body),
    Vs = lists:map(fun(Fld) ->
		couchbeam_doc:get_value(Fld, Doc)
	end, Flds),
	list_to_tuple(Vs).

%%============================================================================
%% Helper functions
%%============================================================================

oauth_get(header, URL, Params, Consumer, Token, TokenSecret) ->
  Signed = oauth:signed_params("GET", URL, Params, Consumer, Token, TokenSecret),
  {AuthorizationParams, QueryParams} = lists:partition(fun({K, _}) -> lists:prefix("oauth_", K) end, Signed),
  Request = {oauth:uri(URL, QueryParams), [oauth:header(AuthorizationParams)]},
  httpc:request(get, Request, [{autoredirect, false}], []);
oauth_get(querystring, URL, Params, Consumer, Token, TokenSecret) ->
  oauth:get(URL, Params, Consumer, Token, TokenSecret).

oauth_post(header, URL, Params, Consumer, Token, TokenSecret) ->
  Signed = oauth:signed_params("POST", URL, Params, Consumer, Token, TokenSecret),
  {AuthorizationParams, QueryParams} = lists:partition(fun({K, _}) -> lists:prefix("oauth_", K) end, Signed),
  Request = {oauth:uri(URL, QueryParams), [oauth:header(AuthorizationParams)], [], []},
  httpc:request(post, Request, [{autoredirect, false}], []);
oauth_post(querystring, URL, Params, Consumer, Token, TokenSecret) ->
  oauth:post(URL, Params, Consumer, Token, TokenSecret).

%%EOF
