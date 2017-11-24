%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 十一月 2017 22:29
%%%-------------------------------------------------------------------
-module(pg_quota_SUITE).
-author("simon").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

-define(APP, pg_quota).

%%=======================================================================

cleanup(_Pid) ->
  db_init(),
  ok.

setup() ->
  pg_test_utils:lager_init(),

  application:start(pg_quota),

  env_init(),

  pg_test_utils:setup(mnesia),

  db_init(),

  ok.

db_init() ->
  RepoContents = [
    {pg_quota:repo_module(mchants),
      [
        [
          {id, 1}
          , {payment_method, [gw_collect]}
          , {sign_method, rsa_hex}
          , {quota, [{txn, 3900}, {daily, 4000}, {monthly, 10000}]}
        ],
        [
          {id, 2}
          , {payment_method, [gw_wap]}
          , {sign_method, rsa_base64}
        ],
        [
          {id, 3}
          , {payment_method, [gw_netbank]}
        ],
        [
          {id, 4}
          , {payment_method, [gw_netbank_only]}
        ]

      ]
    },
    {pg_quota:repo_module(mcht_txn_acc),
      [

      ]
    }
  ],

  pg_test_utils:db_init(RepoContents),
  ok.
%%---------------------------------------------------------------
env_init() ->
  Cfgs = [
    {?APP,
      [
        {mcht_repo_name, pg_quota_t_repo_mchants_pt}
        , {mcht_txn_acc_repo_name, pg_quota_t_repo_mcht_txn_acc_pt}

      ]
    }
  ],

  pg_test_utils:env_init(Cfgs),
  ok.
%%---------------------------------------------------------------
my_test_() ->
  {
    setup
    , fun setup/0
    , fun cleanup/1
    ,
    {
      inorder,
      [
        fun pg_quota_srv:acc_test_1/0

      ]
    }
  }.


