-module(capi_handler_invoice_templates).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).
-export([process_request/3]).
-import(capi_handler_utils, [general_error/2, logic_error/2]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context()
) ->
    {ok | error, capi_handler:response() | noimpl}.

process_request('CreateInvoiceTemplate', Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    ExtraProperties = capi_handler_utils:get_extra_properties(Context),
    try
        CallArgs = [encode_invoice_tpl_create_params(PartyID, maps:get('InvoiceTemplateCreateParams', Req))],
        capi_handler_utils:service_call_with(
            [user_info, party_creation],
            {invoice_templating, 'Create', CallArgs},
            Context
        )
    of
        {ok, InvoiceTpl} ->
            {ok, {201, #{}, make_invoice_tpl_and_token(InvoiceTpl, PartyID, ExtraProperties)}};
        {exception, Exception} ->
            case Exception of
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #payproc_ShopNotFound{} ->
                    {ok, logic_error(invalidShopID, <<"Shop not found">>)};
                #payproc_InvalidPartyStatus{} ->
                    {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                #payproc_InvalidShopStatus{} ->
                    {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)}
            end
    catch
        throw:invoice_cart_empty ->
            {ok, logic_error(invalidInvoiceCart, <<"Wrong size. Path to item: cart">>)};
        throw:zero_invoice_lifetime ->
            {ok, logic_error(invalidRequest, <<"Lifetime cannot be zero">>)}
    end;

process_request('GetInvoiceTemplateByID', Req, Context) ->
    Call = {invoice_templating, 'Get', [maps:get('invoiceTemplateID', Req)]},
    case capi_handler_utils:service_call_with([user_info, party_creation], Call, Context) of
        {ok, InvoiceTpl} ->
            {ok, {200, #{}, decode_invoice_tpl(InvoiceTpl)}};
        {exception, E} when
            E == #payproc_InvalidUser{};
            E == #payproc_InvoiceTemplateNotFound{};
            E == #payproc_InvoiceTemplateRemoved{}
        ->
            {ok, general_error(404, <<"Invoice template not found">>)}
    end;

process_request('UpdateInvoiceTemplate', Req, Context) ->
    try
        Params = encode_invoice_tpl_update_params(maps:get('InvoiceTemplateUpdateParams', Req)),
        Call = {invoice_templating, 'Update', [maps:get('invoiceTemplateID', Req), Params]},
        capi_handler_utils:service_call_with([user_info, party_creation], Call, Context)
    of
        {ok, InvoiceTpl} ->
            {ok, {200, #{}, decode_invoice_tpl(InvoiceTpl)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice Template not found">>)};
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #payproc_InvalidPartyStatus{} ->
                    {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                #payproc_InvalidShopStatus{} ->
                    {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)};
                #payproc_InvoiceTemplateNotFound{} ->
                    {ok, general_error(404, <<"Invoice Template not found">>)};
                #payproc_InvoiceTemplateRemoved{} ->
                    {ok, general_error(404, <<"Invoice Template not found">>)}
            end
    catch
        throw:#payproc_InvalidUser{} ->
            {ok, general_error(404, <<"Invoice Template not found">>)};
        throw:#payproc_InvoiceTemplateNotFound{} ->
            {ok, general_error(404, <<"Invoice Template not found">>)};
        throw:#payproc_InvoiceTemplateRemoved{} ->
            {ok, general_error(404, <<"Invoice Template not found">>)};
        throw:invoice_cart_empty ->
            {ok, logic_error(invalidInvoiceCart, <<"Wrong size. Path to item: cart">>)};
        throw:zero_invoice_lifetime ->
            {ok, logic_error(invalidRequest, <<"Lifetime cannot be zero">>)}
    end;

process_request('DeleteInvoiceTemplate', Req, Context) ->
    Call = {invoice_templating, 'Delete', [maps:get('invoiceTemplateID', Req)]},
    case capi_handler_utils:service_call_with([user_info, party_creation], Call, Context) of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice Template not found">>)};
                #payproc_InvalidPartyStatus{} ->
                    {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                #payproc_InvalidShopStatus{} ->
                    {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)};
                #payproc_InvoiceTemplateNotFound{} ->
                    {ok, general_error(404, <<"Invoice Template not found">>)};
                #payproc_InvoiceTemplateRemoved{} ->
                    {ok, general_error(404, <<"Invoice Template not found">>)}
            end
    end;

process_request('CreateInvoiceWithTemplate' = OperationID, Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    InvoiceTplID = maps:get('invoiceTemplateID', Req),
    InvoiceParams = maps:get('InvoiceParamsWithTemplate', Req),
    ExtraProperties = capi_handler_utils:get_extra_properties(Context),
    try create_invoice(PartyID, InvoiceTplID, InvoiceParams, Context, OperationID) of
        {ok, #'payproc_Invoice'{invoice = Invoice}} ->
            {ok, {201, #{}, capi_handler_decoder_invoicing:make_invoice_and_token(
                Invoice, capi_handler_utils:get_party_id(Context), ExtraProperties)
            }};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice Template not found">>)};
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #payproc_InvalidPartyStatus{} ->
                    {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                #payproc_InvalidShopStatus{} ->
                    {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)};
                #payproc_InvoiceTemplateNotFound{} ->
                    {ok, general_error(404, <<"Invoice Template not found">>)};
                #payproc_InvoiceTemplateRemoved{} ->
                    {ok, general_error(404, <<"Invoice Template not found">>)}
            end
    catch
        throw:{bad_invoice_params, currency_no_amount} ->
            {ok, logic_error(invalidRequest, <<"Amount is required for the currency">>)};
        throw:{bad_invoice_params, amount_no_currency} ->
            {ok, logic_error(invalidRequest, <<"Currency is required for the amount">>)};
        throw:{external_id_conflict, InvoiceID, ExternalID} ->
            {ok, logic_error(externalIDConflict, {InvoiceID, ExternalID})}
    end;

process_request('GetInvoicePaymentMethodsByTemplateID', Req, Context) ->
    InvoiceTemplateID = maps:get('invoiceTemplateID', Req),
    Timestamp = capi_utils:unwrap(rfc3339:format(erlang:system_time())),
    {ok, Party} = capi_handler_utils:get_my_party(Context),
    Revision = Party#domain_Party.revision,
    Args = [InvoiceTemplateID, Timestamp, {revision, Revision}],
    case capi_handler_decoder_invoicing:construct_payment_methods(invoice_templating, Args, Context) of
        {ok, PaymentMethods0} when is_list(PaymentMethods0) ->
            PaymentMethods = capi_utils:deduplicate_payment_methods(PaymentMethods0),
            {ok, {200, #{}, PaymentMethods}};
        {exception, E} when
            E == #payproc_InvalidUser{};
            E == #payproc_InvoiceTemplateNotFound{};
            E == #payproc_InvoiceTemplateRemoved{}
        ->
            {ok, general_error(404, <<"Invoice template not found">>)}
    end;

%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

create_invoice(PartyID, InvoiceTplID, InvoiceParams, #{woody_context := WoodyCtx} = Context, BenderPrefix) ->
    ExternalID = maps:get(<<"externalID">>, InvoiceParams, undefined),
    % CAPI#344: Since the prefixes are different, it's possible to create 2 copies of the same Invoice with the same
    % externalId by using `CreateInvoice` and `CreateInvoiceWithTemplate` together
    IdempotentKey = capi_bender:get_idempotent_key(BenderPrefix, PartyID, ExternalID),
    Hash = erlang:phash2({InvoiceTplID, InvoiceParams}),
    BenderParams = #{
        params_hash => Hash,
        feature_schema => invoice,
        feature_values => InvoiceParams
    },

    case capi_bender:gen_by_snowflake(IdempotentKey, BenderParams, WoodyCtx) of
        {ok, InvoiceID} ->
            Params = [encode_invoice_params_with_tpl(InvoiceID, InvoiceTplID, InvoiceParams)],
            Call = {invoicing, 'CreateWithTemplate', Params},
            capi_handler_utils:service_call_with([user_info, party_creation], Call, Context);
        {error, {external_id_conflict, ID}} ->
            throw({external_id_conflict, ID, ExternalID});
        {error, {external_id_conflict, ID, Difference}} ->
            logger:warning("externalID used in another request.~nDifference: ~p", [Difference]),
            throw({external_id_conflict, ID, ExternalID})
    end.

encode_invoice_tpl_create_params(PartyID, Params) ->
    Details = encode_invoice_tpl_details(genlib_map:get(<<"details">>, Params)),
    Product = get_product_from_tpl_details(Details),
    #payproc_InvoiceTemplateCreateParams{
        party_id         = PartyID,
        shop_id          = genlib_map:get(<<"shopID">>, Params),
        invoice_lifetime = encode_lifetime(Params),
        product          = Product,
        description      = genlib_map:get(<<"description">>, Params),
        details          = Details,
        context          = capi_handler_encoder:encode_invoice_context(Params)
    }.

encode_invoice_tpl_update_params(Params) ->
    Details = encode_invoice_tpl_details(genlib_map:get(<<"details">>, Params)),
    Product = get_product_from_tpl_details(Details),
    #payproc_InvoiceTemplateUpdateParams{
        invoice_lifetime = encode_lifetime(Params),
        product          = Product,
        description      = genlib_map:get(<<"description">>, Params),
        details          = Details,
        context          = encode_optional_context(Params)
    }.

make_invoice_tpl_and_token(InvoiceTpl, PartyID, ExtraProperties) ->
    #{
        <<"invoiceTemplate"           >> => decode_invoice_tpl(InvoiceTpl),
        <<"invoiceTemplateAccessToken">> =>
            capi_handler_utils:issue_access_token(
                PartyID,
                {invoice_tpl, InvoiceTpl#domain_InvoiceTemplate.id},
                ExtraProperties
            )
    }.

encode_invoice_tpl_details(#{<<"templateType">> := <<"InvoiceTemplateSingleLine">>} = Details) ->
    {product, encode_invoice_tpl_product(Details)};
encode_invoice_tpl_details(#{<<"templateType">> := <<"InvoiceTemplateMultiLine">>} = Details) ->
    {cart, capi_handler_encoder:encode_invoice_cart(Details)};
encode_invoice_tpl_details(undefined) ->
    undefined.

get_product_from_tpl_details({product, #domain_InvoiceTemplateProduct{product = Product}}) ->
    Product;
get_product_from_tpl_details({cart, #domain_InvoiceCart{lines = [FirstLine | _]}}) ->
    #domain_InvoiceLine{product = Product} = FirstLine,
    Product;
get_product_from_tpl_details(undefined) ->
    undefined.

encode_optional_context(Params = #{<<"metadata">> := _}) ->
    capi_handler_encoder:encode_invoice_context(Params);
encode_optional_context(#{}) ->
    undefined.

encode_lifetime(#{<<"lifetime">> := Lifetime}) ->
    encode_lifetime(
        genlib_map:get(<<"days">>, Lifetime),
        genlib_map:get(<<"months">>, Lifetime),
        genlib_map:get(<<"years">>, Lifetime)
    );
encode_lifetime(_) ->
    undefined.

encode_lifetime(0, 0, 0) ->
    throw(zero_invoice_lifetime);
encode_lifetime(DD, MM, YY) ->
    #domain_LifetimeInterval{
        days   = DD,
        months = MM,
        years  = YY
      }.

encode_invoice_params_with_tpl(InvoiceID, InvoiceTplID, InvoiceParams) ->
    #payproc_InvoiceWithTemplateParams{
        id          = InvoiceID,
        external_id = genlib_map:get(<<"externalID">>, InvoiceParams),
        template_id = InvoiceTplID,
        cost        = encode_optional_invoice_cost(InvoiceParams),
        context     = encode_optional_context(InvoiceParams)
    }.

encode_invoice_tpl_product(Details) ->
    #domain_InvoiceTemplateProduct{
        product = genlib_map:get(<<"product">>, Details),
        price = encode_invoice_tpl_line_cost(genlib_map:get(<<"price">>, Details)),
        metadata = capi_handler_encoder:encode_invoice_line_meta(Details)
    }.

encode_optional_invoice_cost(Params = #{<<"amount">> := _, <<"currency">> := _}) ->
    capi_handler_encoder:encode_cash(Params);
encode_optional_invoice_cost(#{<<"amount">> := _}) ->
    throw({bad_invoice_params, amount_no_currency});
encode_optional_invoice_cost(#{<<"currency">> := _}) ->
    throw({bad_invoice_params, currency_no_amount});
encode_optional_invoice_cost(_) ->
    undefined.

encode_invoice_tpl_line_cost(#{<<"costType">> := CostType} = Cost) ->
    encode_invoice_tpl_line_cost(CostType, Cost);
encode_invoice_tpl_line_cost(_) ->
    undefined.

encode_invoice_tpl_line_cost(<<"InvoiceTemplateLineCostUnlim">>, _Cost) ->
    {unlim, #domain_InvoiceTemplateCostUnlimited{}};
encode_invoice_tpl_line_cost(<<"InvoiceTemplateLineCostFixed">>, Cost) ->
    {fixed, capi_handler_encoder:encode_cash(Cost)};
encode_invoice_tpl_line_cost(<<"InvoiceTemplateLineCostRange">>, Cost) ->
    Range = genlib_map:get(<<"range">>, Cost),
    {range, #domain_CashRange{
        lower = {inclusive, capi_handler_encoder:encode_cash(
            Cost#{<<"amount">> => genlib_map:get(<<"lowerBound">>, Range)}
        )},
        upper = {inclusive, capi_handler_encoder:encode_cash(
            Cost#{<<"amount">> => genlib_map:get(<<"upperBound">>, Range)}
        )}
    }}.

decode_invoice_tpl(InvoiceTpl) ->
    #domain_LifetimeInterval{days = DD, months = MM, years = YY} = InvoiceTpl#domain_InvoiceTemplate.invoice_lifetime,
    genlib_map:compact(#{
        <<"id"         >> => InvoiceTpl#domain_InvoiceTemplate.id,
        <<"shopID"     >> => InvoiceTpl#domain_InvoiceTemplate.shop_id,
        <<"description">> => InvoiceTpl#domain_InvoiceTemplate.description,
        <<"lifetime"   >> =>
            #{
                <<"days"  >> => undef_to_zero(DD),
                <<"months">> => undef_to_zero(MM),
                <<"years" >> => undef_to_zero(YY)
            },
        <<"details"    >> => decode_invoice_tpl_details(InvoiceTpl#domain_InvoiceTemplate.details),
        <<"metadata"   >> => capi_handler_decoder_utils:decode_context(InvoiceTpl#domain_InvoiceTemplate.context)
    }).

undef_to_zero(undefined) -> 0;
undef_to_zero(Int      ) -> Int.

decode_invoice_tpl_details({cart, Cart}) ->
    #{
        <<"templateType">> => <<"InvoiceTemplateMultiLine">>,
        <<"currency"    >> => get_currency_from_cart(Cart),
        <<"cart"        >> => capi_handler_decoder_invoicing:decode_invoice_cart(Cart)
    };
decode_invoice_tpl_details({product, Product}) ->
    genlib_map:compact(#{
        <<"templateType">> => <<"InvoiceTemplateSingleLine">>,
        <<"product"     >> => Product#domain_InvoiceTemplateProduct.product,
        <<"price"       >> => decode_invoice_tpl_line_cost(Product#domain_InvoiceTemplateProduct.price),
        <<"taxMode"     >> => capi_handler_decoder_invoicing:decode_invoice_line_tax_mode(
            Product#domain_InvoiceTemplateProduct.metadata
        )
    }).

get_currency_from_cart(#domain_InvoiceCart{lines = [FirstLine | _]}) ->
    #domain_InvoiceLine{price = #domain_Cash{currency = Currency}} = FirstLine,
    capi_handler_decoder_utils:decode_currency(Currency).

decode_invoice_tpl_line_cost({unlim, _}) ->
    #{
        <<"costType">> => <<"InvoiceTemplateLineCostUnlim">>
    };

decode_invoice_tpl_line_cost({fixed, #domain_Cash{amount = Amount, currency = Currency}}) ->
    #{
        <<"costType">> => <<"InvoiceTemplateLineCostFixed">>,
        <<"currency">> => capi_handler_decoder_utils:decode_currency(Currency),
        <<"amount">> => Amount
    };

decode_invoice_tpl_line_cost({range, #domain_CashRange{upper = {_, UpperCashBound}, lower = {_, LowerCashBound}}}) ->
    #domain_Cash{amount = UpperBound, currency = Currency} = UpperCashBound,
    #domain_Cash{amount = LowerBound, currency = Currency} = LowerCashBound,
    #{
        <<"costType">> => <<"InvoiceTemplateLineCostRange">>,
        <<"currency">> => capi_handler_decoder_utils:decode_currency(Currency),
        <<"range">> => #{
            <<"upperBound">> => UpperBound,
            <<"lowerBound">> => LowerBound
        }
    }.
