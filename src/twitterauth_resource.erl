-module(twitterauth_resource).
-export([init/1]).
-export([resource_exists/2,previously_existed/2,moved_temporarily/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
	tm_util:connect_db(),
	{ok,undefined}.

resource_exists(ReqData, State) ->
	{false,ReqData,State}.

previously_existed(ReqData, State) ->
	{true,ReqData,State}.

moved_temporarily(ReqData, State) ->
	case wrq:get_qs_value("oauth_token", ReqData) of
	undefined ->
		User = list_to_binary(wrq:get_qs_value("u", ReqData)),

		URL = "http://twitter.com/oauth/request_token",
		{ok,RToken,RTokenSecret} = get_request_token(URL, [], header, tm_conf:consumer()),

		AuthDoc = {[{<<"_id">>,list_to_binary(RToken)},
					{<<"secret">>,list_to_binary(RTokenSecret)},
					{<<"user">>,User},
					{<<"type">>,<<"twitter_auth">>},
					{<<"timestamp">>,tm_util:now()}]},
		couchbeam_db:save_doc(tm_db, AuthDoc),

		AuthURL = oauth:uri("http://twitter.com/oauth/authorize", [{"oauth_token",RToken}]),
		{{true,AuthURL},ReqData,State};

	RToken ->
		case couchbeam_db:open_doc(tm_db, list_to_binary(RToken)) of
		not_found ->
			{{true,"/"},ReqData,State};
		AuthDoc ->
			User = couchbeam_doc:get_value("user", AuthDoc),
			RTokenSecret = binary_to_list(couchbeam_doc:get_value("secret", AuthDoc)),

			URL = "http://twitter.com/oauth/access_token",
			{ok, AToken, ATokenSecret} = get_access_token(URL,
				[], header, tm_conf:consumer(), RToken, RTokenSecret),
			couchbeam_db:delete_doc(tm_db, AuthDoc),

			UserDoc = couchbeam_db:open_doc(tm_db, User),
			TwitDoc = {[{<<"access_token">>,list_to_binary(AToken)},
						{<<"access_token_secret">>,list_to_binary(ATokenSecret)},
						{<<"timestamp">>,tm_util:now()}]},
			UserDoc1 = couchbeam_doc:set_value("twitter_auth", TwitDoc, UserDoc),
			couchbeam_db:save_doc(tm_db, UserDoc1),

			io:format("#### Twitter authorization for ~p~n", [User]),

			Location = "/",
			{{true,Location},ReqData,State}
		end
	end.

get_access_token(URL, Params, ParamsMethod, Consumer, RToken, RTokenSecret) ->
  case oauth_get(ParamsMethod, URL, Params, Consumer, RToken, RTokenSecret) of
    {ok, Response} ->
      case oauth_http:response_code(Response) of
        200 ->
          AParams = oauth_http:response_params(Response),
          {ok, oauth:token(AParams), oauth:token_secret(AParams)};
        _ ->
          Response
      end;
    Error ->
      Error
  end.

get_request_token(URL, Params, ParamsMethod, Consumer) ->
  case oauth_get(ParamsMethod, URL, Params, Consumer, "", "") of
    {ok, Response} ->
      case oauth_http:response_code(Response) of
        200 ->
          RParams = oauth_http:response_params(Response),
          {ok, oauth:token(RParams), oauth:token_secret(RParams)};
        _ ->
		  Response
      end;
    Error ->
	  Error
  end.
 
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

