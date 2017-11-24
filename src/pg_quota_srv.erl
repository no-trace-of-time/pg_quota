%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc 处理商户交易限额相关逻辑
%%%
%%% @end
%%% Created : 24. Nov 2016 11:31 AM
%%%-------------------------------------------------------------------
-module(pg_quota_srv).
-author("simon").
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0
  , check/3
  , update/4
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


%% for UT
-export([
  acc_test_1/0
]).

-define(SERVER, ?MODULE).

-record(state, {}).


-define(QUOTA_UNLIMITED, -1).
-type acc() :: non_neg_integer().
-type mcht_acc() :: {Txn :: acc(), Daily :: acc(), Monthly :: acc()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


-spec check(MchtId, TxnType, TxnAmt) -> pass | out_of_quota when
  MchtId :: pg_mcht_protocol:mcht_id(),
  TxnType :: pg_mcht_protocol:txn_type(),
  TxnAmt :: pg_mcht_protocol:txn_amt().

check(MchtId, TxnType, TxnAmt) when is_binary(MchtId) ->
  check(binary_to_integer(MchtId), TxnType, TxnAmt);
check(MchtId, TxnType, TxnAmt) when is_integer(MchtId) ->
  gen_server:call(?SERVER, {check, MchtId, TxnType, TxnAmt}).

update(MchtId, TxnType, TxnDate, TxnAmt) when is_binary(MchtId) ->
  update(binary_to_integer(MchtId), TxnType, TxnDate, TxnAmt);
update(MchtId, TxnType, TxnDate, TxnAmt) when is_integer(MchtId) ->
  gen_server:cast(?SERVER, {update, MchtId, TxnType, TxnDate, TxnAmt}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({check, MchtId, TxnType, TxnAmt}, _From, State) ->
  Acc = get_txn_acc(MchtId, TxnType),
  Quota = get_mcht_quota(MchtId),
  QuotaCheckResult = check_quota(TxnAmt, Acc, Quota),
  lager:debug("Quota Check Result = ~p,TxnAmt = ~p,Acc=~p,Quota=~p", [QuotaCheckResult, TxnAmt, Acc, Quota]),
  {reply, QuotaCheckResult, State};
handle_call({update, MchtId, TxnType, TxnDate, Amt},
    _From, State) ->
  lager:debug("Before quota update,acc=~p", [get_txn_acc(MchtId, TxnType)]),
  update_acc(MchtId, TxnType, TxnDate, Amt),
  lager:debug("Quota update:MchtId = ~p,TxnType = ~p,TxnDate=~p,TxnAmt=~p", [MchtId, TxnType, TxnDate, Amt]),
  lager:debug("After quota update,acc=~p", [get_txn_acc(MchtId, TxnType)]),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({update, MchtId, TxnType, TxnDate, Amt}, State) ->
  lager:debug("Before quota update,acc=~p", [get_txn_acc(MchtId, TxnType)]),
  update_acc(MchtId, TxnType, TxnDate, Amt),
  lager:debug("Quota update:MchtId = ~p,TxnType = ~p,TxnDate=~p,TxnAmt=~p", [MchtId, TxnType, TxnDate, Amt]),
  lager:debug("After quota update,acc=~p", [get_txn_acc(MchtId, TxnType)]),
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_txn_acc(MchtId, TxnType) -> TxnAcc when
  MchtId :: pg_mcht_protocol:mcht_id(),
  TxnType :: pg_mcht_protocol:txn_type(),
  TxnAcc :: mcht_acc().

get_txn_acc(MchtId, TxnType) ->
  {AccIndexToday, AccIndexMonth} = acc_index_today(MchtId, TxnType),

  MRepoMchtTxnAcc = pg_quota:repo_module(mcht_txn_acc),
  {ok, RepoAccToday} = pg_repo:fetch(MRepoMchtTxnAcc, AccIndexToday),
  AccToday = return_acc(RepoAccToday),
  {ok, RepoAccMonth} = pg_repo:fetch(MRepoMchtTxnAcc, AccIndexMonth),
  AccMonth = return_acc(RepoAccMonth),

  {AccToday, AccMonth}.

%% not found acc record,
return_acc([]) ->
  0;
return_acc([AccRecord]) ->
  MRepoMchtTxnAcc = pg_quota:repo_module(mcht_txn_acc),
  Acc = pg_model:get(MRepoMchtTxnAcc, AccRecord, acc),
  Acc.

get_mcht_quota(MchtId) ->
  MRepoMchants = pg_quota:repo_module(mchants),
  {ok, [Mchant]} = pg_repo:fetch(MRepoMchants, MchtId),
  QuotaPropList = pg_model:get(MRepoMchants, Mchant, quota),
  QuotaTxn = proplists:get_value(txn, QuotaPropList),
  QuotaDaily = proplists:get_value(daily, QuotaPropList),
  QuotaMonthly = proplists:get_value(monthly, QuotaPropList),
  {QuotaTxn, QuotaDaily, QuotaMonthly}.

check_quota(TxnAmt, {AccToday, AccMonth}, {TxnQuota, DailyQuota, MonthlyQuota})
  when is_integer(AccToday), is_integer(AccMonth),
  is_integer(TxnQuota), is_integer(MonthlyQuota), is_integer(DailyQuota) ->
  IsTxnPass = (TxnQuota =:= ?QUOTA_UNLIMITED) or (TxnAmt =< TxnQuota),
  IsDailyPass = (DailyQuota =:= ?QUOTA_UNLIMITED) or (TxnAmt + AccToday =< DailyQuota),
  IsMonthlyPass = (MonthlyQuota =:= ?QUOTA_UNLIMITED) or (TxnAmt + AccMonth =< MonthlyQuota),
  case IsTxnPass and IsDailyPass and IsMonthlyPass of
    true ->
      pass
    ;
    false ->
      out_of_quota
  end.

acc_index_today(MchtId, TxnType) ->
  Today = xfutils:today(),
  <<Month:6/bytes, _/binary>> = Today,
  IndexToday = {MchtId, TxnType, Today},
  IndexMonth = {MchtId, TxnType, Month},
  {IndexToday, IndexMonth}.

update_acc(MchtId, TxnType, TxnDate, Amt)
  when is_integer(MchtId), is_binary(TxnDate), is_integer(Amt) ->

  {AccIndexToday, AccIndexMonth} = acc_index_today(MchtId, TxnType),

  MRepoAcc = pg_quota:repo_module(mcht_txn_acc),
  {ok, AccToday} = pg_repo:fetch(MRepoAcc, AccIndexToday),
  update_acc(AccToday, AccIndexToday, Amt),
  {ok, AccMonth} = pg_repo:fetch(MRepoAcc, AccIndexMonth),
  update_acc(AccMonth, AccIndexMonth, Amt),
  ok.


update_acc([], AccIndex, Amt) ->
  MRepoAcc = pg_quota:repo_module(mcht_txn_acc),
  {MchtId, TxnType, MonthDate} = AccIndex,

  AccRepo = pg_model:new(MRepoAcc,
    [
      {acc_index, AccIndex}
      , {mcht_id, MchtId}
      , {txn_type, TxnType}
      , {month_date, MonthDate}
      , {acc, Amt}
    ]),
  ok = pg_repo:save(AccRepo);

update_acc([Acc], _AccIndex, Amt) ->
  MRepoAcc = pg_quota:repo_module(mcht_txn_acc),
  AccNew = pg_model:inc(MRepoAcc, Acc, acc, Amt),
  ok = pg_repo:save(AccNew).


%%------------------------------------------------------------------
acc_test_1() ->
  MMchtAcc = pg_quota:repo_module(mcht_txn_acc),
  Today = xfutils:today(),
  update(1, collect, Today, 50),
  update(1, collect, Today, 150),


  timer:sleep(100),

  {AccIndexDay, AccIndexMonth} = acc_index_today(1, collect),
  lager:debug("AccIndexDay = ~p,AccIndexMonth=~p", [AccIndexDay, AccIndexMonth]),
  ?assertEqual(200, pg_model:get(MMchtAcc, hd(pg_repo:read(MMchtAcc, AccIndexDay)), acc)),
  ?assertEqual(200, pg_model:get(MMchtAcc, hd(pg_repo:read(MMchtAcc, AccIndexMonth)), acc)),

  ?assertEqual(out_of_quota, check(1, collect, 3801)),
  ?assertEqual(out_of_quota, check(1, collect, 3901)),
  ?assertEqual(pass, check(1, collect, 3700)),

  ok.
