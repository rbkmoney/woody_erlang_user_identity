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
    put_missing_required_error_test/1
]).

%%
%% tests descriptions
%%
all() ->
    [
        put_get_ok_test,
        put_get_incomplete_ok_test,
        put_missing_required_error_test
    ].

%%
%% starting/stopping
%%
init_per_suite(C) ->
    {ok, Apps} = application:ensure_all_started(woody),
    [{apps, Apps}|C].

end_per_suite(C) ->
    [application_stop(App) || App <- proplists:get_value(apps, C)].

application_stop(App) ->
    application:stop(App).

%%
%% tests
%%

-spec put_get_ok_test(_) -> _.
put_get_ok_test(_) ->
    Context0 = woody_context:new(),
    Identity = #{
        id => <<"UserID">>,
        email => <<"UserEmail">>,
        username => <<"UserName">>
    },
    Context = woody_user_identity:put(Identity, Context0),
    Identity = woody_user_identity:get(Context).

-spec put_get_incomplete_ok_test(_) -> _.
put_get_incomplete_ok_test(_) ->
    Context0 = woody_context:new(),
    Identity = #{id => <<"UserID">>},
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
