-module(capi_client_payment_institutions).

-export([get_payment_institutions/1]).
-export([get_payment_institutions/3]).
-export([get_payment_institution_by_ref/2]).
-export([get_payment_institution_payment_terms/2]).

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
