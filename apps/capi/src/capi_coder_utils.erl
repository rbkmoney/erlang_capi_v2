-module(capi_coder_utils).

%% Module for encoding/decoding of various entities

-export([
    encode_country_code/1,
    decode_country_code/1
]).

-spec encode_country_code(binary() | undefined) -> atom().
encode_country_code(undefined) ->
    undefined;
encode_country_code(Code) when is_binary(Code) ->
    try
        binary_to_existing_atom(string:lowercase(Code))
    catch
        error:badarg ->
            throw({encode_country_code, invalid_country_code})
    end.

-spec decode_country_code(atom() | undefined) -> binary().
decode_country_code(undefined) ->
    undefined;
decode_country_code(Code) when is_atom(Code) ->
    list_to_binary(string:to_upper(atom_to_list(Code))).
