%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 十一月 2017 22:32
%%%-------------------------------------------------------------------
-module(pg_quota).
-author("simon").
-include_lib("mixer/include/mixer.hrl").

%% API
-export([
  repo_module/1
]).

-mixin([{pg_quota_srv, [
  check/3
  , update/4
]}]).

-define(APP, pg_quota).
%%==================================================================
repo_module(mchants) ->
  {ok, Module} = application:get_env(?APP, mcht_repo_name),
  mchants = pg_model:name(Module),
  Module;
repo_module(mcht_txn_acc) ->
  {ok, Module} = application:get_env(?APP, mcht_txn_acc_repo_name),
  mcht_txn_acc = pg_model:name(Module),
  Module.
