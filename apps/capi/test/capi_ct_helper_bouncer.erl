-module(capi_ct_helper_bouncer).

-include_lib("common_test/include/ct.hrl").
-include_lib("capi_dummy_data.hrl").
-include_lib("capi_bouncer_data.hrl").

-export([mock_bouncer_client/1]).
-export([mock_bouncer_arbiter/2]).
-export([judge_always_allowed/0]).

%%

start_bouncer_client(ServiceURLs) ->
    ServiceClients = maps:map(fun(_, URL) -> #{url => URL} end, ServiceURLs),
    Acc = application:get_env(bouncer_client, service_clients, #{}),
    capi_ct_helper:start_app(bouncer_client, [{service_clients, maps:merge(Acc, ServiceClients)}]).

-spec mock_bouncer_client(_) -> _.
mock_bouncer_client(SupOrConfig) ->
    start_bouncer_client(
        capi_ct_helper:mock_services_(
            [
                {
                    org_management,
                    {orgmgmt_auth_context_provider_thrift, 'AuthContextProvider'},
                    fun('GetUserContext', {UserID}) ->
                        {encoded_fragment, Fragment} = bouncer_client:bake_context_fragment(
                            bouncer_context_helpers:make_user_fragment(#{
                                id => UserID,
                                realm => #{id => ?TEST_USER_REALM},
                                orgs => [#{id => ?STRING, owner => #{id => UserID}, party => #{id => UserID}}]
                            })
                        ),
                        {ok, Fragment}
                    end
                }
            ],
            SupOrConfig
        )
    ).

-spec mock_bouncer_arbiter(_, _) -> _.
mock_bouncer_arbiter(JudgeFun, SupOrConfig) ->
    start_bouncer_client(
        capi_ct_helper:mock_services_(
            [
                {
                    bouncer,
                    {bouncer_decisions_thrift, 'Arbiter'},
                    fun('Judge', {?TEST_RULESET_ID, Context}) ->
                        Fragments = decode_bouncer_context(Context),
                        Combined = combine_fragments(Fragments),
                        JudgeFun(Combined)
                    end
                }
            ],
            SupOrConfig
        )
    ).

decode_bouncer_context(#bdcs_Context{fragments = Fragments}) ->
    maps:map(fun(_, Fragment) -> decode_bouncer_fragment(Fragment) end, Fragments).

decode_bouncer_fragment(#bctx_ContextFragment{type = v1_thrift_binary, content = Content}) ->
    Type = {struct, struct, {bouncer_context_v1_thrift, 'ContextFragment'}},
    Codec = thrift_strict_binary_codec:new(Content),
    {ok, Fragment, _} = thrift_strict_binary_codec:read(Codec, Type),
    Fragment.

-spec judge_always_allowed() -> _.
judge_always_allowed() ->
    fun(_) -> {ok, ?JUDGEMENT(?ALLOWED)} end.

combine_fragments(Fragments) ->
    [Fragment | Rest] = maps:values(Fragments),
    lists:foldl(fun combine_fragments/2, Fragment, Rest).

combine_fragments(Fragment1 = #bctx_v1_ContextFragment{}, Fragment2 = #bctx_v1_ContextFragment{}) ->
    combine_records(Fragment1, Fragment2).

combine_records(Record1, Record2) ->
    [Tag | Fields1] = tuple_to_list(Record1),
    [Tag | Fields2] = tuple_to_list(Record2),
    list_to_tuple([Tag | lists:zipwith(fun combine_fragment_fields/2, Fields1, Fields2)]).

combine_fragment_fields(undefined, V) ->
    V;
combine_fragment_fields(V, undefined) ->
    V;
combine_fragment_fields(V, V) ->
    V;
combine_fragment_fields(V1, V2) when is_tuple(V1), is_tuple(V2) ->
    combine_records(V1, V2);
combine_fragment_fields(V1, V2) when is_list(V1), is_list(V2) ->
    ordsets:union(V1, V2).
