-module(capi_handler_utils).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-export([general_error/1]).
-export([logic_error/2]).
-export([reply_5xx/1]).
-export([format_request_errors/1]).

-export([service_call_with/3]).
-export([service_call/2]).

-export([get_my_party/1]).

-export([issue_access_token/2]).
-export([merge_and_compact/2]).
-export([get_time/2]).
-export([get_split_interval/2]).
-export([get_time_diff/2]).
-export([collect_events/5]).

-export([unwrap_payment_session/1]).
-export([wrap_payment_session/2]).
-export([get_invoice_by_id/2]).
-export([get_payment_by_id/3]).

-type processing_context() :: capi_handler:processing_context().

-spec general_error(binary()) ->
    map().

general_error(Message) ->
    #{<<"message">> => genlib:to_binary(Message)}.

-spec logic_error(integer(), binary()) ->
    map().

logic_error(Code, Message) ->
    #{<<"code">> => genlib:to_binary(Code), <<"message">> => genlib:to_binary(Message)}.

-spec reply_5xx(integer()) ->
    {integer(), [], <<>>}.

reply_5xx(Code) when Code >= 500 andalso Code < 600 ->
    {Code, [], <<>>}.

-spec format_request_errors(list()) ->
    binary().

format_request_errors([]    ) -> <<>>;
format_request_errors(Errors) -> genlib_string:join(<<"\n">>, Errors).

%%%

% Нужно быть аккуратным с флагами их порядок влияет на порядок аргументов при вызове функций!
% обычно параметры идут в порядке [user_info, party_id, party_creation],
% но это зависит от damsel протокола
-spec service_call_with(list(atom()), tuple(), processing_context()) ->
    woody:result().

service_call_with(Flags, Call, Context) ->
    % реверс тут чтобы в флагах писать порядок аналогично вызову функций
    service_call_with_(lists:reverse(Flags), Call, Context).

service_call_with_([user_info|T], {ServiceName, Function, Args}, Context) ->
    service_call_with_(T, {ServiceName, Function, [get_user_info(Context) | Args]}, Context);
service_call_with_([party_id|T], {ServiceName, Function, Args}, Context) ->
    service_call_with_(T, {ServiceName, Function, [get_party_id(Context) | Args]}, Context);
service_call_with_([party_creation|T], Call, Context) ->
    case service_call_with_(T, Call, Context) of
        {exception, #payproc_PartyNotFound{}} ->
            _ = lager:info("Attempting to create a missing party"),
            CreateCall = {party_management, 'Create', [get_party_params(Context)]},
            case service_call_with([user_info, party_id], CreateCall, Context) of
                {ok       , _                     } -> service_call_with_(T, Call, Context);
                {exception, #payproc_PartyExists{}} -> service_call_with_(T, Call, Context);
                Error                               -> Error
            end;
        Result ->
            Result
    end;
service_call_with_([], Call, Context) ->
    service_call(Call, Context).

-spec service_call(tuple(), processing_context()) ->
    woody:result().

service_call({ServiceName, Function, Args}, #{woody_context := WoodyContext}) ->
    capi_woody_client:call_service(ServiceName, Function, Args, WoodyContext).

get_party_params(Context) ->
    #payproc_PartyParams{
        contact_info = #domain_PartyContactInfo{
            email = capi_auth:get_claim(<<"email">>, get_auth_context(Context))
        }
    }.

get_auth_context(#{swagger_context := #{auth_context := AuthContext}}) ->
    AuthContext.

get_user_info(Context) ->
    #payproc_UserInfo{
        id = get_party_id(Context),
        type = {external_user, #payproc_ExternalUser{}}
    }.

get_party_id(Context) ->
    capi_auth:get_subject_id(get_auth_context(Context)).

%% Common functions

-spec get_my_party(processing_context()) ->
    woody:result().

get_my_party(Context) ->
    Call = {party_management, 'Get', []},
    service_call_with([user_info, party_id, party_creation], Call, Context).

%% Utils

-spec issue_access_token(binary(), tuple()) ->
    map().

issue_access_token(PartyID, TokenSpec) ->
    #{<<"payload">> => capi_auth:issue_access_token(PartyID, TokenSpec)}.

-spec merge_and_compact(map(), map()) ->
    map().

merge_and_compact(M1, M2) ->
    genlib_map:compact(maps:merge(M1, M2)).

-spec get_time(atom(), map()) ->
    TimestampUTC :: binary() | undefined.

get_time(Key, Req) ->
    case genlib_map:get(Key, Req) of
        Timestamp when is_binary(Timestamp) ->
            capi_utils:to_universal_time(Timestamp);
        undefined ->
            undefined
    end.

-spec get_split_interval(integer(), atom()) ->
    integer().

get_split_interval(SplitSize, minute) -> SplitSize * 60;
get_split_interval(SplitSize, hour  ) -> get_split_interval(SplitSize, minute) * 60;
get_split_interval(SplitSize, day   ) -> get_split_interval(SplitSize, hour  ) * 24;
get_split_interval(SplitSize, week  ) -> get_split_interval(SplitSize, day   ) * 7;
get_split_interval(SplitSize, month ) -> get_split_interval(SplitSize, day   ) * 30;
get_split_interval(SplitSize, year  ) -> get_split_interval(SplitSize, day   ) * 365.

-spec get_time_diff(binary(), binary()) ->
    integer().

get_time_diff(From, To) ->
    {DateFrom, TimeFrom} = parse_rfc3339_datetime(From),
    {DateTo, TimeTo} = parse_rfc3339_datetime(To),
    UnixFrom = genlib_time:daytime_to_unixtime({DateFrom, TimeFrom}),
    UnixTo = genlib_time:daytime_to_unixtime({DateTo, TimeTo}),
    UnixTo - UnixFrom.

parse_rfc3339_datetime(DateTime) ->
    {ok, {DateFrom, TimeFrom, _, _}} = rfc3339:parse(DateTime),
    {DateFrom, TimeFrom}.

-spec collect_events(_, _, _, _, _) ->
    _.

collect_events(Limit, After, GetterFun, DecodeFun, Context) ->
    collect_events([], Limit, After, GetterFun, DecodeFun, Context).

collect_events(Collected, 0, _, _, _, _) ->
    {ok, Collected};

collect_events(Collected0, Left, After, GetterFun, DecodeFun, Context) when Left > 0 ->
    case get_events(Left, After, GetterFun) of
        {ok, Events} ->
            Filtered = decode_and_filter_events(DecodeFun, Context, Events),
            Collected = Collected0 ++ Filtered,
            case length(Events) of
                Left ->
                    collect_events(
                        Collected,
                        Left - length(Filtered),
                        get_last_event_id(Events),
                        GetterFun,
                        DecodeFun,
                        Context
                    );
                N when N < Left ->
                    {ok, Collected}
            end;
        Error ->
            Error
    end.

decode_and_filter_events(DecodeFun, Context, Events) ->
    lists:foldr(
        fun(Event, Acc) ->
             case DecodeFun(Event, Context) of
                {true, Ev} ->
                    [Ev|Acc];
                false ->
                    Acc
            end
        end,
        [],
        Events
    ).

get_last_event_id(Events) ->
    #payproc_Event{
        id = ID
    } = lists:last(Events),
    ID.

get_events(Limit, After, GetterFun) ->
    EventRange = #'payproc_EventRange'{
        limit = Limit,
        'after' = After
    },
    GetterFun(EventRange).

-spec unwrap_payment_session(_) ->
    {_, _}.

unwrap_payment_session(Encoded) ->
    #{
        <<"clientInfo">> := ClientInfo,
        <<"paymentSession">> := PaymentSession
     } = try
            capi_utils:base64url_to_map(Encoded)
        catch
            error:badarg ->
                erlang:throw(invalid_payment_session)
        end,
    {ClientInfo, PaymentSession}.

-spec wrap_payment_session(_, _) ->
    _.

wrap_payment_session(ClientInfo, PaymentSession) ->
    capi_utils:map_to_base64url(#{
        <<"clientInfo"    >> => ClientInfo,
        <<"paymentSession">> => PaymentSession
    }).

-spec get_invoice_by_id(_, _) ->
    _.

get_invoice_by_id(InvoiceID, Context) ->
    service_call_with([user_info], {invoicing, 'Get', [InvoiceID]}, Context).

-spec get_payment_by_id(_, _, _) ->
    _.

get_payment_by_id(InvoiceID, PaymentID, Context) ->
    service_call_with([user_info], {invoicing, 'GetPayment', [InvoiceID, PaymentID]}, Context).
