-module(woody_user_identity).

%% API exports
-export([put/2]).
-export([get/1]).

-type id()       :: binary().
-type email()    :: binary().
-type username() :: binary().

-type user_identity() :: #{
    id := id(),
    email => email(),
    username => username()
}.

-export_type([id/0]).
-export_type([email/0]).
-export_type([username/0]).
-export_type([user_identity/0]).

-define(PREFIX, <<"user_identity.">>).

%%====================================================================
%% API functions
%%====================================================================

-spec put(user_identity(), woody_context:ctx()) -> woody_context:ctx() | no_return().
put(Identity, Context) ->
    Meta = prepare_meta(Identity),
    woody_context:add_meta(Context, Meta).

-spec get(woody_context:ctx()) -> user_identity().
get(Context) ->
    get_meta(Context).

%%====================================================================
%% Internal functions
%%====================================================================

prepare_meta(Identity) ->
    lists:foldl(
        fun({Key, Rules}, Acc) ->
            Required = is_required(Rules),
            case maps:get(Key, Identity, undefined) of
                undefined when Required =:= false->
                    Acc;
                undefined ->
                    throw({missing_required, Key});
                Value ->
                    MetaKey = encode_key(Key),
                    maps:put(MetaKey, Value, Acc)
            end
        end,
        #{},
        get_keys_info()
    ).

get_meta(Context) ->
    lists:foldl(
        fun({Key, _Rules}, Acc) ->
            MetaKey = encode_key(Key),
            case woody_context:get_meta(MetaKey, Context) of
                undefined ->
                    Acc;
                Value ->
                    maps:put(Key, Value, Acc)
            end
        end,
        #{},
        get_keys_info()
    ).

get_keys_info() ->
    [
        {id, [{required, true}]},
        {email, []},
        {username, []}
    ].

is_required(Rules) ->
    case proplists:get_value(required, Rules, false) of
        true ->
            true;
        _ ->
            false
    end.

encode_key(Key) when is_atom(Key) ->
    <<(?PREFIX)/binary, (genlib:to_binary(Key))/binary>>.
