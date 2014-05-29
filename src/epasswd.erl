%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% 
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%% 
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%% 
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%% 
%%% @doc
%%%
%%% @end
%%% Created : 27 May 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(epasswd).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 auth/1,
	 share_group/2]).

-callback init(Opts :: term()) ->
    {ok, State :: term()} |
    {error, Reason :: term()}.

-callback auth(Credentials :: term(), State :: term()) ->
    true | false.

-callback share_group(User1 :: term(), User2 :: term(), State :: term()) ->
    true | false.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {mod      :: atom(),
		state    :: term()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec auth(term()) -> true | false.
auth(Credentials) ->
    gen_server:call(?SERVER, {auth, Credentials}).

-spec share_group(term(), term()) -> true | false.
share_group(User1, User2) ->
    gen_server:call(?SERVER, {share_group, User1, User2}).

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
init([]) ->
    case application:get_env(mod) of
	{ok, {Mod, Opts}} ->
	    RealMod = list_to_atom("epasswd_mod_" ++ atom_to_list(Mod)),
	    case is_module(RealMod) of
		true -> 
		    try RealMod:init(Opts) of
			State -> 
			    {ok, #state{mod=RealMod, state=State}}
		    catch throw:Err ->
			    {stop, Err}
		    end;
		false ->
		    {stop, {invalid_module, Mod}}
	    end;
	undefined ->
	    {ok, #state{}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({auth, Credentials}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:auth(Credentials, S) of
	{Ret, S2} ->
	    {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
	    {stop, Err, State}
    end;
handle_call({share_group, User1, User2}, _From, #state{mod=Mod, state=S}=State) ->
    try Mod:share_group(User1, User2, S) of
	{Ret, S2} ->
	    {reply, Ret, State#state{state=S2}}
    catch throw:Err ->
	    {stop, Err, State}
    end;    
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
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
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
is_module(Mod) when is_atom(Mod) ->
    try Mod:module_info() of
	_ -> true
    catch _:_ -> false
    end;
is_module(_) -> 
    false.
