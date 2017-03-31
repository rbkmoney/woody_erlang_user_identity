-module(woody_user_identity).

%% API exports
-export([]).

-type id()       :: binary().
-type email()    :: binary().
-type username() :: binary().

-type user_identity() :: #{
    id => id(),
    email => email(),
    username => username()
}.

-define(PREFIX, <<"user_identity_">>).

-export([put/2]).
-export([get/1]).

%%====================================================================
%% API functions
%%====================================================================
-spec put(user_identity(), woody_context:ctx()) -> woody_context:ctx() | no_return().
put(Identity, Context) ->
    Meta = prepare_meta(Identity),
    _ = ensure_not_override(Meta, Context),
    woody_context:add_meta(Context, Meta).

-spec get(woody_context:ctx()) -> user_identity().
get(Context) ->
    get_meta(Context).

%%====================================================================
%% Internal functions
%%====================================================================

prepare_meta(Identity) ->
    lists:foldl(
        fun(Key, Acc) ->
            case maps:get(Key, Identity, undefined) of
                undefined ->
                    Acc;
                Value ->
                    MetaKey = encode_key(Key),
                    maps:put(MetaKey, Value, Acc)
            end
        end,
        #{},
        keys()
    ).

get_meta(Context) ->
    lists:foldl(
        fun(Key, Acc) ->
            MetaKey = encode_key(Key),
            case woody_context:get_meta(MetaKey, Context) of
                undefined ->
                    Acc;
                Value ->
                    maps:put(Key, Value, Acc)
            end
        end,
        #{},
        keys()
    ).

keys() ->
    [
        id,
        email,
        username
    ].

encode_key(Key) when is_atom(Key) ->
    <<(?PREFIX)/binary, (genlib:to_binary(Key))/binary>>.

decode_key(MetaKey) when is_binary(MetaKey) ->
    PrefixSize = byte_size(?PREFIX),
    <<_:PrefixSize/binary, Key/binary>> = MetaKey,
    binary_to_existing_atom(Key, utf8).

ensure_not_override(Meta, Context) ->
    genlib_map:foreach(
        fun(MetaKey, _) ->
            case woody_context:get_meta(MetaKey, Context) of
                undefined ->
                    ok;
                _ ->
                    throw({override_attempt, decode_key(MetaKey)})
            end
        end,
        Meta
    ).
