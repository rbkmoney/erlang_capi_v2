-module(capi_handler_call).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export([service_call_with/3]).
-export([service_call/2]).

-export([get_party/1]).
-export([get_party/2]).
-export([get_invoice_by_id/2]).
-export([get_payment_by_id/3]).
-export([get_refund_by_id/4]).
-export([get_contract_by_id/2]).
-export([get_contract_by_id/3]).
-export([get_shop_by_id/2]).
-export([get_shop_by_id/3]).

-type processing_context() :: capi_handler:processing_context().

% Нужно быть аккуратным с флагами их порядок влияет на порядок аргументов при вызове функций!
% обычно параметры идут в порядке [user_info, party_id],
% но это зависит от damsel протокола
-spec service_call_with(list(atom()), {atom(), atom(), tuple()}, processing_context()) -> woody:result().
service_call_with(Flags, Call, Context) ->
    % реверс тут чтобы в флагах писать порядок аналогично вызову функций
    service_call_with_(lists:reverse(Flags), Call, Context).

service_call_with_([user_info | T], {ServiceName, Function, Args}, Context) ->
    service_call_with_(
        T,
        {ServiceName, Function, prepend_tuple(capi_handler_utils:get_user_info(Context), Args)},
        Context
    );
service_call_with_([party_id | T], {ServiceName, Function, Args}, Context) ->
    service_call_with_(
        T,
        {ServiceName, Function, prepend_tuple(capi_handler_utils:get_party_id(Context), Args)},
        Context
    );
service_call_with_([], Call, Context) ->
    service_call(Call, Context).

-spec service_call({atom(), atom(), tuple()}, capi_handler:processing_context()) -> woody:result().
service_call({ServiceName, Function, Args}, #{woody_context := WoodyContext}) ->
    capi_woody_client:call_service(ServiceName, Function, Args, WoodyContext).

-spec get_party(processing_context()) -> woody:result().
get_party(Context) ->
    Call = {party_management, 'Get', {}},
    service_call_with([user_info, party_id], Call, Context).

-spec get_party(binary(), processing_context()) -> woody:result().
get_party(PartyID, Context) ->
    Call = {party_management, 'Get', {PartyID}},
    service_call_with([user_info], Call, Context).

-spec get_invoice_by_id(binary(), processing_context()) -> woody:result().
get_invoice_by_id(InvoiceID, Context) ->
    EventRange = #payproc_EventRange{},
    Args = {InvoiceID, EventRange},
    service_call_with([user_info], {invoicing, 'Get', Args}, Context).

-spec get_payment_by_id(binary(), binary(), processing_context()) -> woody:result().
get_payment_by_id(InvoiceID, PaymentID, Context) ->
    service_call_with([user_info], {invoicing, 'GetPayment', {InvoiceID, PaymentID}}, Context).

-spec get_refund_by_id(binary(), binary(), binary(), processing_context()) -> woody:result().
get_refund_by_id(InvoiceID, PaymentID, RefundID, Context) ->
    service_call_with([user_info], {invoicing, 'GetPaymentRefund', {InvoiceID, PaymentID, RefundID}}, Context).

-spec get_contract_by_id(binary(), processing_context()) -> woody:result().
get_contract_by_id(ContractID, Context) ->
    Call = {party_management, 'GetContract', {ContractID}},
    service_call_with([user_info, party_id], Call, Context).

-spec get_contract_by_id(binary(), binary(), processing_context()) -> woody:result().
get_contract_by_id(PartyID, ContractID, Context) ->
    Call = {party_management, 'GetContract', {PartyID, ContractID}},
    service_call_with([user_info], Call, Context).

-spec get_shop_by_id(binary(), processing_context()) -> woody:result().
get_shop_by_id(ShopID, Context) ->
    Call = {party_management, 'GetShop', {ShopID}},
    service_call_with([user_info, party_id], Call, Context).

-spec get_shop_by_id(binary(), binary(), processing_context()) -> woody:result().
get_shop_by_id(PartyID, ShopID, Context) ->
    Call = {party_management, 'GetShop', {PartyID, ShopID}},
    service_call_with([user_info], Call, Context).

-spec prepend_tuple(any(), tuple()) -> tuple().
prepend_tuple(Item, Tuple) ->
    erlang:insert_element(1, Tuple, Item).
