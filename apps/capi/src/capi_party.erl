-module(capi_party).

-export([create_party/3]).
-export([get_party/2]).
-export([checkout_party/3]).
-export([activate_party/2]).
-export([suspend_party/2]).
-export([get_contract/3]).
-export([get_shop/3]).
-export([get_shop_contract/3]).
-export([activate_shop/3]).
-export([suspend_shop/3]).
-export([compute_payment_institution_terms/3]).
-export([get_claim/3]).
-export([get_claims/2]).
-export([get_account_state/3]).

-type result() :: ok | {ok, woody:result()} | {error, woody_error:business_error()}.
-type processing_context() :: capi_handler:processing_context().

-type party_id() :: party_client_thrift:party_id().
-type party_params() :: party_client_thrift:party_params().
-type party_revision_param() :: party_client_thrift:party_revision_param().
-type payment_institution_ref() :: party_client_thrift:payment_institution_ref().
-type varset() :: party_client_thrift:varset().
-type contract_id() :: party_client_thrift:contract_id().
-type shop_id() :: party_client_thrift:shop_id().
-type claim_id() :: party_client_thrift:claim_id().
-type account_id() :: party_client_thrift:account_id().

-spec create_party(party_id(), party_params(), processing_context()) -> result().
create_party(PartyID, PartyParams, Context) ->
    {Client, ClientContext} = client_context(Context),
    party_client_thrift:create(PartyID, PartyParams, Client, ClientContext).

-spec get_party(party_id(), processing_context()) -> result().
get_party(PartyID, Context) ->
    {Client, ClientContext} = client_context(Context),
    party_client_thrift:get(PartyID, Client, ClientContext).

-spec checkout_party(party_id(), party_revision_param(), processing_context()) -> result().
checkout_party(PartyID, PartyRevision, Context) ->
    {Client, ClientContext} = client_context(Context),
    party_client_thrift:checkout(PartyID, PartyRevision, Client, ClientContext).

-spec activate_party(party_id(), processing_context()) -> result().
activate_party(PartyID, Context) ->
    {Client, ClientContext} = client_context(Context),
    party_client_thrift:activate(PartyID, Client, ClientContext).

-spec suspend_party(party_id(), processing_context()) -> result().
suspend_party(PartyID, Context) ->
    {Client, ClientContext} = client_context(Context),
    party_client_thrift:suspend(PartyID, Client, ClientContext).

-spec get_contract(party_id(), contract_id(), processing_context()) -> result().
get_contract(PartyID, ContractID, Context) ->
    {Client, ClientContext} = client_context(Context),
    party_client_thrift:get_contract(PartyID, ContractID, Client, ClientContext).

-spec get_shop(party_id(), shop_id(), processing_context()) -> result().
get_shop(PartyID, ShopID, Context) ->
    {Client, ClientContext} = client_context(Context),
    party_client_thrift:get_shop(PartyID, ShopID, Client, ClientContext).

-spec get_shop_contract(party_id(), shop_id(), processing_context()) -> result().
get_shop_contract(PartyID, ShopID, Context) ->
    {Client, ClientContext} = client_context(Context),
    party_client_thrift:get_shop_contract(PartyID, ShopID, Client, ClientContext).

-spec activate_shop(party_id(), shop_id(), processing_context()) -> result().
activate_shop(PartyID, ShopID, Context) ->
    {Client, ClientContext} = client_context(Context),
    party_client_thrift:activate_shop(PartyID, ShopID, Client, ClientContext).

-spec suspend_shop(party_id(), shop_id(), processing_context()) -> result().
suspend_shop(PartyID, ShopID, Context) ->
    {Client, ClientContext} = client_context(Context),
    party_client_thrift:suspend_shop(PartyID, ShopID, Client, ClientContext).

-spec compute_payment_institution_terms(
    payment_institution_ref(),
    varset(),
    processing_context()
) -> result().
compute_payment_institution_terms(Ref, Varset, Context) ->
    {Client, ClientContext} = client_context(Context),
    party_client_thrift:compute_payment_institution_terms(
        Ref,
        Varset,
        Client,
        ClientContext
    ).

-spec get_claim(party_id(), claim_id(), processing_context()) -> result().
get_claim(PartyID, ClaimID, Context) ->
    {Client, ClientContext} = client_context(Context),
    party_client_thrift:get_claim(PartyID, ClaimID, Client, ClientContext).

-spec get_claims(party_id(), processing_context()) -> result().
get_claims(PartyID, Context) ->
    {Client, ClientContext} = client_context(Context),
    party_client_thrift:get_claims(PartyID, Client, ClientContext).

-spec get_account_state(party_id(), account_id(), processing_context()) -> result().
get_account_state(PartyID, AccountID, Context) ->
    {Client, ClientContext} = client_context(Context),
    party_client_thrift:get_account_state(PartyID, AccountID, Client, ClientContext).

client_context(#{party_client := Client, party_client_context := ClientContext}) ->
    {Client, ClientContext}.
