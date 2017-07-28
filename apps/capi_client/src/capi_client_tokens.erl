-module(capi_client_tokens).

-export([create_payment_tool_token/2]).

-type context() :: capi_client_lib:context().

-spec create_payment_tool_token(context(), map()) -> {ok, term(), term()} | {error, term()}.
create_payment_tool_token(Context, Request) ->
    Params = #{body => Request},
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_tokens_api:create_payment_tool_token(Url, PreparedParams, Opts),
    case capi_client_lib:handle_response(Response) of
        {ok, #{<<"token">> := Token, <<"session">> := Session}} ->
            {ok, Token, Session};
        {error, Error} -> {error, Error}
    end.
