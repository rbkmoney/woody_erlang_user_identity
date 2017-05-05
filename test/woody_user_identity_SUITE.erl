-module(woody_user_identity_SUITE).

-include_lib("common_test/include/ct.hrl").


%% common test API
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    put_get_ok_test/1,
    put_get_incomplete_ok_test/1,
    put_missing_required_error_test/1,
    get_missing_required_error_test/1
]).

%%
%% tests descriptions
%%

-spec all() -> _.
all() ->
    [
        put_get_ok_test,
        put_get_incomplete_ok_test,
        put_missing_required_error_test,
        get_missing_required_error_test
    ].

%%
%% starting/stopping
%%

-spec init_per_suite(_) -> _.
init_per_suite(C) ->
    {ok, Apps} = application:ensure_all_started(woody),
    [{apps, Apps}|C].

-spec end_per_suite(_) -> _.
end_per_suite(C) ->
    [application:stop(App) || App <- proplists:get_value(apps, C)].

%%
%% tests
%%

-spec put_get_ok_test(_) -> _.
put_get_ok_test(_) ->
    Context0 = woody_context:new(),
    Identity = #{
        id => <<"UserID">>,
        email => <<"UserEmail">>,
        username => <<"UserName">>,
        realm => <<"TestRealm">>
    },
    Context = woody_user_identity:put(Identity, Context0),
    Identity = woody_user_identity:get(Context).

-spec put_get_incomplete_ok_test(_) -> _.
put_get_incomplete_ok_test(_) ->
    Context0 = woody_context:new(),
    Identity = #{id => <<"UserID">>, realm => <<"TestRealm">>},
    Context = woody_user_identity:put(Identity, Context0),
    Identity = woody_user_identity:get(Context).

-spec put_missing_required_error_test(_) -> _.
put_missing_required_error_test(_) ->
    Context0 = woody_context:new(),
    Identity = #{email => <<"test@test.com">>},
    ok = try
        woody_user_identity:put(Identity, Context0),
        error
    catch
        throw:{missing_required, id} ->
            ok
    end.

-spec get_missing_required_error_test(_) -> _.
get_missing_required_error_test(_) ->
    Context = woody_context:new(),
    ok = try
        woody_user_identity:get(Context),
        error
    catch
        throw:{missing_required, id} ->
            ok
    end.
