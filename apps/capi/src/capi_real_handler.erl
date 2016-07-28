-module(capi_real_handler).

-behaviour(swagger_logic_handler).

%% API callbacks
-export([handle_request/2]).
-export([authorize_api_key/2]).

-spec authorize_api_key(ApiKey :: binary(), OperationID :: atom()) -> Result :: boolean() | {boolean(), #{binary() => any()}}.
authorize_api_key(ApiKey, OperationID) -> capi_auth:auth_api_key(ApiKey, OperationID).

-spec handle_request(OperationID :: atom(), Req :: #{}) -> {Code :: integer, Headers :: [], Response :: #{}}.
handle_request('CreateInvoice', _Req) ->
    {501, [], <<"Not implemented">>};

handle_request('CreatePayment', _Req) ->
    {501, [], <<"Not implemented">>};

handle_request('CreatePaymentToolToken', _Req) ->
    {501, [], <<"Not implemented">>};

handle_request('GetInvoiceByID', _Req) ->
    {501, [], <<"Not implemented">>};

handle_request('GetInvoiceEvents', _Req) ->
    {501, [], <<"Not implemented">>};

handle_request('GetPaymentByID', _Req) ->
    {501, [], <<"Not implemented">>};

handle_request(_OperationID, _Req) ->
    {501, [], <<"Not implemented">>}.
