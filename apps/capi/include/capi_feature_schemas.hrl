-ifndef(__capi_feature_schemas__).
-define(__capi_feature_schemas__, 42).

%% macros with special semantic _MACRO_,
%% use in idempotent features algorithm.
-define(_type_,           1). % affect on compare feature algorithm
-define(invoice_id,       2).
-define(make_recurrent,   3).
-define(flow,             4).
-define(hold_exp,         5).
-define(payer,            6).
-define(tool,             7).
-define(token,            8).
-define(bank_card,        9).
-define(expdate,         10).
-define(terminal,        11).
-define(terminal_type,   12).
-define(wallet,          13).
-define(provider,        14).
-define(id,              15).
-define(crypto,          16).
-define(currency,        17).
-define(mobile_commerce, 18).
-define(operator,        19).
-define(phone,           20).
-define(customer,        21).
-define(recurrent,       22).
-define(invoice,         23).
-define(payment,         24).
-define(shop_id,         25).
-define(amount,          26).
-define(product,         27).
-define(due_date,        28).
-define(cart,            29).
-define(quantity,        30).
-define(price,           31).
-define(tax,             32).
-define(rate,            33).

-endif.
