-module(capi_client_payment_institutions).

-export([get_payment_institutions/1]).
-export([get_payment_institutions/3]).
-export([get_payment_institution_by_ref/2]).
-export([get_payment_institution_payment_terms/2]).
-export([get_payment_institution_payout_methods/2]).
-export([get_payment_institution_payout_methods/3]).
-export([get_payment_institution_payout_schedules/2]).
-export([get_payment_institution_payout_schedules/4]).

-type context() :: capi_client_lib:context().

-spec get_payment_institutions(context()) -> {ok, term()} | {error, term()}.
get_payment_institutions(Context) ->
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, #{}),
    Response = swag_client_payment_institutions_api:get_payment_institutions(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payment_institutions(context(), binary(), binary()) -> {ok, term()} | {error, term()}.
get_payment_institutions(Context, Residence, Realm) ->
    Params = #{
        qs_val => genlib_map:compact(#{
            residence => Residence,
            realm => Realm
        })
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payment_institutions_api:get_payment_institutions(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payment_institution_by_ref(context(), term()) -> {ok, term()} | {error, term()}.
get_payment_institution_by_ref(Context, PaymentInstitutionRef) ->
    Params = #{
        binding => #{
            <<"paymentInstitutionID">> => genlib:to_list(PaymentInstitutionRef)
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payment_institutions_api:get_payment_institution_by_ref(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payment_institution_payment_terms(context(), term()) -> {ok, term()} | {error, term()}.
get_payment_institution_payment_terms(Context, PaymentInstitutionID) ->
    Params = #{
        binding => #{
            <<"paymentInstitutionID">> => genlib:to_list(PaymentInstitutionID)
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payment_institutions_api:get_payment_institution_payment_terms(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payment_institution_payout_methods(context(), term()) -> {ok, term()} | {error, term()}.
get_payment_institution_payout_methods(Context, PaymentInstitutionID) ->
    Params = #{
        binding => #{
            <<"paymentInstitutionID">> => genlib:to_list(PaymentInstitutionID)
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payment_institutions_api:get_payment_institution_payout_methods(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payment_institution_payout_methods(context(), term(), term()) -> {ok, term()} | {error, term()}.
get_payment_institution_payout_methods(Context, PaymentInstitutionID, Currency) ->
    Params = #{
        binding => #{
            <<"paymentInstitutionID">> => genlib:to_list(PaymentInstitutionID)
        },
        qs_val => #{
            currency => Currency
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payment_institutions_api:get_payment_institution_payout_methods(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payment_institution_payout_schedules(context(), term()) -> {ok, term()} | {error, term()}.
get_payment_institution_payout_schedules(Context, PaymentInstitutionID) ->
    Params = #{
        binding => #{
            <<"paymentInstitutionID">> => genlib:to_list(PaymentInstitutionID)
        }
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payment_institutions_api:get_payment_institution_payout_schedules(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).

-spec get_payment_institution_payout_schedules(context(), term(), term(), term()) -> {ok, term()} | {error, term()}.
get_payment_institution_payout_schedules(Context, PaymentInstitutionID, Currency, Method) ->
    Params = #{
        binding => #{
            <<"paymentInstitutionID">> => genlib:to_list(PaymentInstitutionID)
        },
        qs_val => genlib_map:compact(#{
            currency => Currency,
            payoutMethod => Method
        })
    },
    {Url, PreparedParams, Opts} = capi_client_lib:make_request(Context, Params),
    Response = swag_client_payment_institutions_api:get_payment_institution_payout_schedules(Url, PreparedParams, Opts),
    capi_client_lib:handle_response(Response).
