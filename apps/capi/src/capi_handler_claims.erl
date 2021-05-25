-module(capi_handler_claims).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-behaviour(capi_handler).

-export([prepare/3]).

-import(capi_handler_utils, [general_error/2, logic_error/2]).

-spec prepare(
    OperationID :: capi_handler:operation_id(),
    Req :: capi_handler:request_data(),
    Context :: capi_handler:processing_context()
) -> {ok, capi_handler:request_state()} | {error, noimpl}.
prepare(OperationID = 'GetClaims', Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{party => PartyID, id => OperationID}}
        ],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        Call = {party_management, 'GetClaims', {PartyID}},
        Claims = capi_utils:unwrap(
            capi_handler_call:service_call_with([user_info], Call, Context)
        ),
        {ok, {200, #{}, decode_claims(filter_claims(maps:get('claimStatus', Req), Claims))}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID = 'GetClaimByID', Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    ClaimID = maps:get('claimID', Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{party => PartyID, claim => ClaimID, id => OperationID}}
        ],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        CallArgs = {PartyID, genlib:to_int(ClaimID)},
        Call = {party_management, 'GetClaim', CallArgs},
        case capi_handler_call:service_call_with([user_info], Call, Context) of
            {ok, Claim} ->
                case is_wallet_claim(Claim) of
                    true ->
                        %% filter this out
                        {ok, general_error(404, <<"Claim not found">>)};
                    false ->
                        {ok, {200, #{}, decode_claim(Claim)}}
                end;
            {exception, #payproc_ClaimNotFound{}} ->
                {ok, general_error(404, <<"Claim not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID = 'CreateClaim', Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    try
        Changeset = encode_claim_changeset(maps:get('ClaimChangeset', Req)),
        Authorize = fun() ->
            Prototypes = [
                {operation, #{party => PartyID, id => OperationID}}
            ],
            {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
        end,
        Process = fun() ->
            CallArgs = {PartyID, Changeset},
            Call = {party_management, 'CreateClaim', CallArgs},
            case capi_handler_call:service_call_with([user_info], Call, Context) of
                {ok, Claim} ->
                    {ok, {201, #{}, decode_claim(Claim)}};
                {exception, Exception} ->
                    case Exception of
                        #payproc_InvalidPartyStatus{} ->
                            {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                        #payproc_ChangesetConflict{} ->
                            {ok, logic_error(changesetConflict, <<"Changeset conflict">>)};
                        #payproc_InvalidChangeset{} ->
                            {ok, logic_error(invalidChangeset, <<"Invalid changeset">>)};
                        #'InvalidRequest'{errors = Errors} ->
                            FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                            {ok, logic_error(invalidRequest, FormattedErrors)}
                    end
            end
        end,
        {ok, #{authorize => Authorize, process => Process}}
    catch
        throw:{encode_contract_modification, adjustment_creation_not_supported} ->
            ErrorResp = logic_error(
                invalidChangeset,
                <<"Contract adjustment creation not supported">>
            ),
            capi_handler:respond(ErrorResp);
        throw:{encode_residence, invalid_residence} ->
            capi_handler:respond(logic_error(invalidRequest, <<"Invalid residence">>))
    end;
% TODO disabled temporary, exception handling must be fixed befor enabling
% prepare(OperationID = 'UpdateClaimByID', Req, Context) ->
%     PartyID = capi_handler_utils:get_party_id(Context),
%     ClaimID = maps:get('claimID', Req),
%     Authorize = fun() ->
%         Prototypes = [
%             {operation, #{party => PartyID, claim => ClaimID, id => OperationID}}
%         ],
%         {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
%     end,
%     Process = fun() ->
%         Call =
%             {party_management, 'UpdateClaim', {
%                 PartyID,
%                 genlib:to_int(ClaimID),
%                 genlib:to_int(maps:get('claimRevision', Req)),
%                 encode_claim_changeset(maps:get('claimChangeset', Req))
%             }},
%         Party = capi_utils:unwrap(
%             capi_handler_call:service_call_with([user_info], Call, Context)
%         ),
%         {ok, {200, #{}, capi_handler_decoder_party:decode_party(Party)}}
%     end,
%     {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID = 'RevokeClaimByID', Req, Context) ->
    PartyID = capi_handler_utils:get_party_id(Context),
    ClaimID = maps:get('claimID', Req),
    Authorize = fun() ->
        Prototypes = [
            {operation, #{party => PartyID, claim => ClaimID, id => OperationID}}
        ],
        {ok, capi_auth:authorize_operation(OperationID, Prototypes, Context, Req)}
    end,
    Process = fun() ->
        CallArgs = {
            PartyID,
            genlib:to_int(ClaimID),
            genlib:to_int(maps:get('claimRevision', Req)),
            encode_reason(maps:get('Reason', Req))
        },
        Call = {party_management, 'RevokeClaim', CallArgs},
        case capi_handler_call:service_call_with([user_info], Call, Context) of
            {ok, _} ->
                {ok, {204, #{}, undefined}};
            {exception, Exception} ->
                case Exception of
                    #payproc_InvalidPartyStatus{} ->
                        {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                    #payproc_ClaimNotFound{} ->
                        {ok, general_error(404, <<"Claim not found">>)};
                    #payproc_InvalidClaimStatus{} ->
                        {ok, logic_error(invalidClaimStatus, <<"Invalid claim status">>)};
                    #payproc_InvalidClaimRevision{} ->
                        {ok, logic_error(invalidClaimRevision, <<"Invalid claim revision">>)}
                end
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

filter_claims(ClaimStatus, Claims) ->
    lists:filter(
        fun(C) ->
            is_claim_status_equals(ClaimStatus, C) andalso
                (not is_wallet_claim(C))
        end,
        Claims
    ).

is_claim_status_equals(undefined, _) ->
    true;
is_claim_status_equals(ClaimStatus, #payproc_Claim{status = {Status, _}}) ->
    Status =:= ClaimStatus.

encode_claim_changeset(Changeset) when is_list(Changeset) ->
    lists:map(fun encode_party_modification/1, Changeset).

encode_party_modification(#{<<"partyModificationType">> := Type} = Modification) ->
    case Type of
        <<"ContractModification">> ->
            {contract_modification, encode_contract_modification(Modification)};
        <<"ShopModification">> ->
            {shop_modification, encode_shop_modification(Modification)}
    end.

encode_contract_modification(#{<<"contractID">> := ContractID} = Modification) ->
    EncodedMod =
        case maps:get(<<"contractModificationType">>, Modification) of
            <<"ContractCreation">> ->
                {creation, #payproc_ContractParams{
                    contractor = encode_contractor(maps:get(<<"contractor">>, Modification)),
                    payment_institution = encode_payment_institution_ref(
                        maps:get(<<"paymentInstitutionID">>, Modification)
                    )
                }};
            <<"ContractTermination">> ->
                {termination, #payproc_ContractTermination{
                    reason = encode_reason(maps:get(<<"reason">>, Modification))
                }};
            <<"ContractLegalAgreementBinding">> ->
                {legal_agreement_binding, encode_legal_agreement(maps:get(<<"legalAgreement">>, Modification))};
            <<"ContractAdjustmentCreation">> ->
                % FIXME need swag supprot for template ref
                %     {adjustment_modification, #payproc_ContractAdjustmentModificationUnit{
                %         adjustment_id = maps:get(<<"adjustmentID">>, Modification),
                %         modification = {creation, #payproc_ContractAdjustmentParams{
                %             template = NOT_SUPPORTED
                %         }}
                %     }};
                erlang:throw({encode_contract_modification, adjustment_creation_not_supported});
            <<"ContractPayoutToolCreation">> ->
                {payout_tool_modification, #payproc_PayoutToolModificationUnit{
                    payout_tool_id = maps:get(<<"payoutToolID">>, Modification),
                    modification = {creation, encode_payout_tool_params(Modification)}
                }};
            <<"ContractPayoutToolInfoModification">> ->
                {payout_tool_modification, #payproc_PayoutToolModificationUnit{
                    payout_tool_id = maps:get(<<"payoutToolID">>, Modification),
                    modification = {info_modification, encode_payout_tool_info(maps:get(<<"details">>, Modification))}
                }};
            <<"ContractReportingPreferencesChange">> ->
                {report_preferences_modification, encode_report_preferences(Modification)}
        end,
    #payproc_ContractModificationUnit{
        id = ContractID,
        modification = EncodedMod
    }.

encode_shop_modification(#{<<"shopID">> := ShopID} = Modification) ->
    EncodedMod =
        case maps:get(<<"shopModificationType">>, Modification) of
            <<"ShopCreation">> ->
                {creation, encode_shop_params(Modification)};
            <<"ShopAccountCreation">> ->
                {shop_account_creation, #payproc_ShopAccountParams{
                    currency = capi_handler_encoder:encode_currency(maps:get(<<"currency">>, Modification))
                }};
            <<"ShopCategoryChange">> ->
                {category_modification, encode_category_ref(maps:get(<<"categoryID">>, Modification))};
            <<"ShopLocationChange">> ->
                {location_modification, encode_shop_location(maps:get(<<"location">>, Modification))};
            <<"ShopDetailsChange">> ->
                {details_modification, encode_shop_details(maps:get(<<"details">>, Modification))};
            <<"ShopContractBinding">> ->
                {contract_modification, #payproc_ShopContractModification{
                    contract_id = maps:get(<<"contractID">>, Modification),
                    payout_tool_id = maps:get(<<"payoutToolID">>, Modification)
                }};
            <<"ShopPayoutToolChange">> ->
                {payout_tool_modification, maps:get(<<"payoutToolID">>, Modification)};
            <<"ShopPayoutScheduleChange">> ->
                {payout_schedule_modification, #payproc_ScheduleModification{
                    schedule = encode_schedule_ref(genlib_map:get(<<"scheduleID">>, Modification))
                }}
        end,
    #payproc_ShopModificationUnit{
        id = ShopID,
        modification = EncodedMod
    }.

encode_reason(undefined) -> undefined;
encode_reason(#{<<"reason">> := Reason}) -> Reason.

encode_legal_agreement(LegalAgreement) ->
    #domain_LegalAgreement{
        signed_at = maps:get(<<"signedAt">>, LegalAgreement),
        legal_agreement_id = maps:get(<<"id">>, LegalAgreement),
        valid_until = genlib_map:get(<<"validUntil">>, LegalAgreement)
    }.

encode_payout_tool_params(#{<<"currency">> := Currency, <<"details">> := Details}) ->
    #payproc_PayoutToolParams{
        currency = capi_handler_encoder:encode_currency(Currency),
        tool_info = encode_payout_tool_info(Details)
    }.

encode_payout_tool_info(#{<<"detailsType">> := <<"PayoutToolDetailsBankAccount">>} = Tool) ->
    {russian_bank_account, encode_russian_bank_account(Tool)};
encode_payout_tool_info(#{<<"detailsType">> := <<"PayoutToolDetailsInternationalBankAccount">>} = Tool) ->
    {international_bank_account, encode_international_bank_account(Tool)};
encode_payout_tool_info(#{<<"detailsType">> := <<"PayoutToolDetailsWalletInfo">>} = Tool) ->
    {wallet_info, #domain_WalletInfo{wallet_id = maps:get(<<"walletID">>, Tool)}}.

encode_russian_bank_account(BankAccount) ->
    #domain_RussianBankAccount{
        account = maps:get(<<"account">>, BankAccount),
        bank_name = maps:get(<<"bankName">>, BankAccount),
        bank_post_account = maps:get(<<"bankPostAccount">>, BankAccount),
        bank_bik = maps:get(<<"bankBik">>, BankAccount)
    }.

encode_international_bank_account(undefined) ->
    undefined;
encode_international_bank_account(Acc) ->
    #domain_InternationalBankAccount{
        iban = genlib_map:get(<<"iban">>, Acc),
        number = genlib_map:get(<<"number">>, Acc),
        bank = encode_international_bank_details(genlib_map:get(<<"bankDetails">>, Acc)),
        correspondent_account = encode_international_bank_account(genlib_map:get(<<"correspondentBankAccount">>, Acc))
    }.

encode_international_bank_details(undefined) ->
    undefined;
encode_international_bank_details(Acc) ->
    #domain_InternationalBankDetails{
        bic = genlib_map:get(<<"bic">>, Acc),
        country = capi_handler_encoder:encode_residence(genlib_map:get(<<"countryCode">>, Acc)),
        name = genlib_map:get(<<"name">>, Acc),
        address = genlib_map:get(<<"address">>, Acc),
        aba_rtn = genlib_map:get(<<"abartn">>, Acc)
    }.

encode_contractor(#{<<"contractorType">> := <<"PrivateEntity">>} = Contractor) ->
    {private_entity, encode_private_entity(Contractor)};
encode_contractor(#{<<"contractorType">> := <<"LegalEntity">>} = Contractor) ->
    {legal_entity, encode_legal_entity(Contractor)};
encode_contractor(#{<<"contractorType">> := <<"RegisteredUser">>} = Contractor) ->
    {registered_user, encode_registered_user(Contractor)}.

encode_private_entity(#{<<"entityType">> := <<"RussianPrivateEntity">>} = Entity) ->
    {russian_private_entity, #domain_RussianPrivateEntity{
        first_name = maps:get(<<"firstName">>, Entity),
        second_name = maps:get(<<"secondName">>, Entity),
        middle_name = maps:get(<<"middleName">>, Entity),
        contact_info = capi_handler_encoder:encode_contact_info(maps:get(<<"contactInfo">>, Entity))
    }}.

encode_legal_entity(#{<<"entityType">> := <<"RussianLegalEntity">>} = Entity) ->
    {russian_legal_entity, #domain_RussianLegalEntity{
        registered_name = maps:get(<<"registeredName">>, Entity),
        registered_number = maps:get(<<"registeredNumber">>, Entity),
        inn = maps:get(<<"inn">>, Entity),
        actual_address = maps:get(<<"actualAddress">>, Entity),
        post_address = maps:get(<<"postAddress">>, Entity),
        representative_position = maps:get(<<"representativePosition">>, Entity),
        representative_full_name = maps:get(<<"representativeFullName">>, Entity),
        representative_document = maps:get(<<"representativeDocument">>, Entity),
        russian_bank_account = encode_russian_bank_account(maps:get(<<"bankAccount">>, Entity))
    }};
encode_legal_entity(#{<<"entityType">> := <<"InternationalLegalEntity">>} = Entity) ->
    {international_legal_entity, #domain_InternationalLegalEntity{
        legal_name = genlib_map:get(<<"legalName">>, Entity),
        trading_name = genlib_map:get(<<"tradingName">>, Entity),
        registered_address = genlib_map:get(<<"registeredOffice">>, Entity),
        actual_address = genlib_map:get(<<"principalPlaceOfBusiness">>, Entity),
        registered_number = genlib_map:get(<<"registeredNumber">>, Entity)
    }}.

encode_registered_user(#{<<"email">> := Email}) ->
    #domain_RegisteredUser{email = Email}.

encode_payment_institution_ref(Ref) ->
    #domain_PaymentInstitutionRef{id = Ref}.

encode_report_preferences(#{
    <<"serviceAcceptanceActPreferences">> := #{
        <<"scheduleID">> := ScheduleID,
        <<"signer">> := Signer
    }
}) ->
    #domain_ReportPreferences{
        service_acceptance_act_preferences = #domain_ServiceAcceptanceActPreferences{
            schedule = encode_schedule_ref(ScheduleID),
            signer = encode_representative(Signer)
        }
    };
encode_report_preferences(_) ->
    #domain_ReportPreferences{}.

encode_representative(Representative) ->
    #domain_Representative{
        position = maps:get(<<"position">>, Representative),
        full_name = maps:get(<<"fullName">>, Representative),
        document = encode_representative_document(maps:get(<<"document">>, Representative))
    }.

encode_representative_document(#{<<"representativeDocumentType">> := <<"ArticlesOfAssociation">>}) ->
    {articles_of_association, #domain_ArticlesOfAssociation{}};
encode_representative_document(#{<<"representativeDocumentType">> := <<"PowerOfAttorney">>} = Document) ->
    {power_of_attorney, encode_legal_agreement(Document)}.

encode_shop_params(Params) ->
    #payproc_ShopParams{
        location = encode_shop_location(genlib_map:get(<<"location">>, Params)),
        details = encode_shop_details(genlib_map:get(<<"details">>, Params)),
        contract_id = genlib_map:get(<<"contractID">>, Params),
        payout_tool_id = genlib_map:get(<<"payoutToolID">>, Params)
    }.

encode_shop_details(undefined) ->
    undefined;
encode_shop_details(Details = #{<<"name">> := Name}) ->
    #domain_ShopDetails{
        name = Name,
        description = genlib_map:get(<<"description">>, Details)
    }.

encode_shop_location(#{
    <<"locationType">> := <<"ShopLocationUrl">>,
    <<"url">> := Url
}) ->
    {url, Url}.

encode_category_ref(undefined) ->
    undefined;
encode_category_ref(Ref) ->
    #domain_CategoryRef{id = Ref}.

encode_schedule_ref(ID) when ID /= undefined ->
    #domain_BusinessScheduleRef{id = ID};
encode_schedule_ref(undefined) ->
    undefined.

%%

decode_claims(Claims) ->
    lists:map(fun decode_claim/1, Claims).

decode_claim(Claim) ->
    capi_handler_utils:merge_and_compact(
        #{
            <<"id">> => Claim#payproc_Claim.id,
            <<"revision">> => Claim#payproc_Claim.revision,
            <<"createdAt">> => Claim#payproc_Claim.created_at,
            <<"updatedAt">> => Claim#payproc_Claim.updated_at,
            <<"changeset">> => decode_party_changeset(Claim#payproc_Claim.changeset)
        },
        decode_claim_status(Claim#payproc_Claim.status)
    ).

is_wallet_claim(#payproc_Claim{changeset = Changeset}) ->
    lists:any(fun is_wallet_change/1, Changeset).

is_wallet_change({contractor_modification, _}) ->
    true;
is_wallet_change({wallet_modification, _}) ->
    true;
is_wallet_change(
    {contract_modification, #payproc_ContractModificationUnit{
        modification =
            {creation, #payproc_ContractParams{
                contractor = undefined
            }}
    }}
) ->
    true;
is_wallet_change(
    {contract_modification, #payproc_ContractModificationUnit{
        modification = {contractor_modification, _}
    }}
) ->
    true;
is_wallet_change(_) ->
    false.

decode_claim_status({'pending', _}) ->
    #{<<"status">> => <<"ClaimPending">>};
decode_claim_status({'accepted', #payproc_ClaimAccepted{}}) ->
    #{<<"status">> => <<"ClaimAccepted">>};
decode_claim_status({'denied', #payproc_ClaimDenied{reason = Reason}}) ->
    #{<<"status">> => <<"ClaimDenied">>, <<"reason">> => Reason};
decode_claim_status({'revoked', #payproc_ClaimRevoked{reason = Reason}}) ->
    #{<<"status">> => <<"ClaimRevoked">>, <<"reason">> => Reason}.

decode_party_changeset(PartyChangeset) ->
    lists:filtermap(fun decode_party_modification/1, PartyChangeset).

decode_party_modification({contract_modification, ContractModification}) ->
    {true,
        maps:merge(
            #{
                <<"partyModificationType">> => <<"ContractModification">>,
                <<"contractID">> => ContractModification#payproc_ContractModificationUnit.id
            },
            decode_contract_modification(ContractModification#payproc_ContractModificationUnit.modification)
        )};
decode_party_modification({shop_modification, ShopModification}) ->
    {true,
        maps:merge(
            #{
                <<"partyModificationType">> => <<"ShopModification">>,
                <<"shopID">> => ShopModification#payproc_ShopModificationUnit.id
            },
            decode_shop_modification(ShopModification#payproc_ShopModificationUnit.modification)
        )}.

decode_contract_modification({creation, ContractParams}) ->
    #{
        <<"contractModificationType">> => <<"ContractCreation">>,
        <<"contractor">> =>
            capi_handler_decoder_party:decode_contractor(ContractParams#payproc_ContractParams.contractor),
        <<"paymentInstitutionID">> =>
            capi_handler_decoder_party:decode_payment_institution_ref(
                ContractParams#payproc_ContractParams.payment_institution
            )
    };
decode_contract_modification({legal_agreement_binding, LegalAgreement}) ->
    #{
        <<"contractModificationType">> => <<"ContractLegalAgreementBinding">>,
        <<"legalAgreement">> => capi_handler_decoder_party:decode_legal_agreement(LegalAgreement)
    };
decode_contract_modification({adjustment_modification, AdjustmentModification}) ->
    #payproc_ContractAdjustmentModificationUnit{
        adjustment_id = AdjustmentID,
        modification =
            {creation, #payproc_ContractAdjustmentParams{
                % FIXME need swag support for this
                template = _Template
            }}
    } = AdjustmentModification,
    #{
        <<"contractModificationType">> => <<"ContractAdjustmentCreation">>,
        <<"adjustmentID">> => AdjustmentID
    };
decode_contract_modification({termination, #payproc_ContractTermination{reason = Reason}}) ->
    genlib_map:compact(#{
        <<"contractModificationType">> => <<"ContractTermination">>,
        <<"reason">> => Reason
    });
decode_contract_modification(
    {payout_tool_modification, #payproc_PayoutToolModificationUnit{
        payout_tool_id = PayoutToolID,
        modification = {creation, PayoutToolParams}
    }}
) ->
    maps:merge(
        #{
            <<"contractModificationType">> => <<"ContractPayoutToolCreation">>,
            <<"payoutToolID">> => PayoutToolID
        },
        decode_payout_tool_params(PayoutToolParams)
    );
decode_contract_modification(
    {payout_tool_modification, #payproc_PayoutToolModificationUnit{
        payout_tool_id = PayoutToolID,
        modification = {info_modification, ToolInfo}
    }}
) ->
    #{
        <<"contractModificationType">> => <<"ContractPayoutToolInfoModification">>,
        <<"payoutToolID">> => PayoutToolID,
        <<"details">> => capi_handler_decoder_party:decode_payout_tool_details(ToolInfo)
    };
decode_contract_modification({report_preferences_modification, ReportPreferences}) ->
    maps:merge(
        #{<<"contractModificationType">> => <<"ContractReportingPreferencesChange">>},
        capi_handler_decoder_party:decode_reporting_preferences(ReportPreferences)
    ).

decode_shop_modification({creation, ShopParams}) ->
    maps:merge(
        #{<<"shopModificationType">> => <<"ShopCreation">>},
        decode_shop_params(ShopParams)
    );
decode_shop_modification({shop_account_creation, #payproc_ShopAccountParams{currency = Currency}}) ->
    #{
        <<"shopModificationType">> => <<"ShopAccountCreation">>,
        <<"currency">> => capi_handler_decoder_utils:decode_currency(Currency)
    };
decode_shop_modification({category_modification, CategoryRef}) ->
    #{
        <<"shopModificationType">> => <<"ShopCategoryChange">>,
        <<"categoryID">> => capi_handler_decoder_utils:decode_category_ref(CategoryRef)
    };
decode_shop_modification({location_modification, Location}) ->
    #{
        <<"shopModificationType">> => <<"ShopLocationChange">>,
        <<"location">> => capi_handler_decoder_party:decode_shop_location(Location)
    };
decode_shop_modification({details_modification, Details}) ->
    #{
        <<"shopModificationType">> => <<"ShopDetailsChange">>,
        <<"details">> => capi_handler_decoder_party:decode_shop_details(Details)
    };
decode_shop_modification({contract_modification, ContractMod}) ->
    #{
        <<"shopModificationType">> => <<"ShopContractBinding">>,
        <<"contractID">> => ContractMod#payproc_ShopContractModification.contract_id,
        <<"payoutToolID">> => ContractMod#payproc_ShopContractModification.payout_tool_id
    };
decode_shop_modification({payout_tool_modification, PayoutToolID}) ->
    #{
        <<"shopModificationType">> => <<"ShopPayoutToolChange">>,
        <<"payoutToolID">> => PayoutToolID
    };
decode_shop_modification({payout_schedule_modification, #payproc_ScheduleModification{schedule = ScheduleRef}}) ->
    genlib_map:compact(#{
        <<"shopModificationType">> => <<"ShopPayoutScheduleChange">>,
        <<"scheduleID">> => capi_handler_decoder_utils:decode_business_schedule_ref(ScheduleRef)
    }).

decode_payout_tool_params(#payproc_PayoutToolParams{currency = Currency, tool_info = ToolInfo}) ->
    capi_handler_decoder_party:decode_payout_tool_params(Currency, ToolInfo).

decode_shop_params(ShopParams) ->
    #{
        <<"location">> => capi_handler_decoder_party:decode_shop_location(ShopParams#payproc_ShopParams.location),
        <<"details">> => capi_handler_decoder_party:decode_shop_details(ShopParams#payproc_ShopParams.details),
        <<"contractID">> => ShopParams#payproc_ShopParams.contract_id,
        <<"payoutToolID">> => ShopParams#payproc_ShopParams.payout_tool_id
    }.
