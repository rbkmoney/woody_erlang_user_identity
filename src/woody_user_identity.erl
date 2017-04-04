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

-define(PREFIX, <<"user-identity.">>).

-type key() :: atom().
-type rules() :: [{required, boolean()}].

%%====================================================================
%% API functions
%%====================================================================

-spec put(user_identity(), woody_context:ctx()) -> woody_context:ctx() | no_return().
put(Identity, Context) ->
    Meta = prepare_meta(Identity),
    woody_context:add_meta(Context, Meta).

-spec get(woody_context:ctx()) -> user_identity() | no_return().
get(Context) ->
    get_user_identity(Context).

%%====================================================================
%% Internal functions
%%====================================================================

-spec prepare_meta(map()) -> map() | no_return().
prepare_meta(Identity) ->
    lists:foldl(
        fun({Key, Rules}, Acc) ->
            Required = is_required(Rules),
            case maps:get(Key, Identity, undefined) of
                undefined when Required =:= false->
                    Acc;
                undefined ->
                    missing_required_error(Key);
                Value ->
                    MetaKey = encode_key(Key),
                    Acc#{MetaKey => Value}
            end
        end,
        #{},
        get_keys_info()
    ).

-spec get_user_identity(woody_context:ctx()) -> user_identity() | no_return().
get_user_identity(Context) ->
    lists:foldl(
        fun({Key, Rules}, Acc) ->
            MetaKey = encode_key(Key),
            Required = is_required(Rules),
            case woody_context:get_meta(MetaKey, Context) of
                undefined when Required =:= false->
                    Acc;
                undefined ->
                    missing_required_error(Key);
                Value ->
                    Acc#{Key => Value}
            end
        end,
        #{},
        get_keys_info()
    ).

-spec get_keys_info() -> [{key(), rules()}].
get_keys_info() ->
    [
        {id, [{required, true}]},
        {email, []},
        {username, []}
    ].

-spec is_required(rules()) -> boolean().
is_required(Rules) ->
    case proplists:get_value(required, Rules, false) of
        true ->
            true;
        _ ->
            false
    end.

-spec encode_key(key()) -> binary().
encode_key(Key) when is_atom(Key) ->
    <<(?PREFIX)/binary, (genlib:to_binary(Key))/binary>>.

-spec missing_required_error(key()) -> no_return().
missing_required_error(Key) ->
    throw({missing_required, Key}).
