-module(capi_bender_generator).

-include_lib("bender_proto/include/bender_thrift.hrl").

-type woody_context()   :: woody_context:ctx().
-type sequence_params() :: #{minimum := integer()}.
-type generated_id()    :: #{
    id => binary(),
    integer_id => integer()
}.


-export([gen_by_snowflake/1]).
-export([gen_by_sequence/3]).
-export([gen_by_constant/2]).

-spec gen_by_snowflake(woody_context()) ->
    generated_id().

gen_by_snowflake(WoodyContext) ->
    Snowflake = {snowflake, #bender_SnowflakeSchema{}},
    generated_id(Snowflake, WoodyContext).

-spec gen_by_sequence(binary(), sequence_params(), woody_context()) ->
    generated_id().

gen_by_sequence(SequenceID, SequenceParams, WoodyContext) ->
    Minimum = maps:get(minimum, SequenceParams, undefined),
    Sequence = {sequence, #bender_SequenceSchema{
        sequence_id = SequenceID,
        minimum = Minimum
    }},
    generated_id(Sequence, WoodyContext).

-spec gen_by_constant(binary(),woody_context()) ->
    generated_id().

gen_by_constant(ConstantID, WoodyContext) ->
    Constant = {constant, #bender_ConstantSchema{internal_id = ConstantID}},
    generated_id(Constant, WoodyContext).

generated_id(Schema, WoodyContext) ->
    {ok, Result} = capi_woody_client:call_service(generator, 'GenerateID', [Schema], WoodyContext),
    #bender_GeneratedID{
        id = ID,
        integer_id = IntegerID
    } = Result,
    #{id => ID, integer_id => IntegerID}.
