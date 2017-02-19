-module(capi_acl_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-type testcase() :: {_, fun()}.

-spec illegal_input_test_()   -> [testcase()].
-spec empty_test_()           -> [testcase()].
-spec stable_encoding_test_() -> [testcase()].
-spec redundancy_test_()      -> [testcase()].
-spec match_scope_test_()     -> [testcase()].

illegal_input_test_() ->
    [
        ?_assertError({badarg, {scope     , _}}, from_list([{[], read}])),
        ?_assertError({badarg, {permission, _}}, from_list([{[invoices], wread}])),
        ?_assertError({badarg, {resource  , _}}, from_list([{[payments], read}]))
    ].

empty_test_() ->
    [
        ?_assertEqual([], encode(from_list([]))),
        ?_assertEqual([], to_list(decode([])))
    ].

stable_encoding_test_() ->
    ACL1 = from_list([
        {[party], read},
        {[party], write},
        {[invoices], read},
        {[invoices, payments], read},
        {[{invoices, <<"42">>}, payments], write}
    ]),
    Enc1 = [
        <<"invoices.42.payments:write">>,
        <<"invoices.*.payments:read">>,
        <<"party:read">>,
        <<"party:write">>,
        <<"invoices:read">>
    ],
    [
        ?_assertEqual(Enc1, encode(ACL1)),
        ?_assertEqual(ACL1, decode(Enc1)),
        ?_assertEqual(ACL1, decode(encode(ACL1)))
    ].

redundancy_test_() ->
    [
        ?_assertEqual([<<"party:read">>], encode(from_list([{[party], read}, {[party], read}])))
    ].

match_scope_test_() ->
    ACL = from_list([
        {[party], read},
        {[party], write},
        {[invoices], read},
        {[invoices, payments], write},
        {[{invoices, <<"42">>}], write},
        {[{invoices, <<"42">>}, payments], read}
    ]),
    [
        ?_assertError({badarg, _}   , match([], ACL)),
        ?_assertEqual([write]       , match([{invoices, <<"42">>}], ACL)),
        ?_assertEqual([read]        , match([{invoices, <<"43">>}], ACL)),
        ?_assertEqual([read]        , match([{invoices, <<"42">>}, {payments, <<"1">>}], ACL)),
        ?_assertEqual([write]       , match([{invoices, <<"43">>}, {payments, <<"1">>}], ACL)),
        ?_assertEqual([read, write] , match([{party, <<"BLARGH">>}], ACL)),
        ?_assertEqual([]            , match([payment_tool_tokens], ACL))
    ].

from_list(L) ->
    capi_acl:from_list(L).

to_list(L) ->
    capi_acl:to_list(L).

match(S, ACL) ->
    capi_acl:match(S, ACL).

encode(ACL) ->
    capi_acl:encode(ACL).

decode(Bin) ->
    capi_acl:decode(Bin).
