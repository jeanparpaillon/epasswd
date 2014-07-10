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
-module(epasswd_mod_xmpp).

-behaviour(epasswd).

-export([init/1,
         auth/2, share_group/3, create_group/2, create_ingroup/3,
         create_user/3, delete_user/2, delete_ingroup/2, delete_group/2, get_groups/1, start/2, update_group/2]).

-record(state, {db, table, lists_group, session}).
-type state() :: #state{}.

-spec init([]) -> state().
init([]) ->
    F = fun() ->
                mnesia:all_keys(occi_user)
        end,
    Table = ets:new(table_user, []),
    Db_Tuple = mnesia:transaction(F),
    Db = element(2, Db_Tuple),
    #state{db = Db, table = Table, lists_group=[]}.

-spec start(Opts :: term(), state()) -> {true | false, state()}.
start(Opts, State) ->
    {true, State#state{session=Opts}}.

-spec auth({User1 :: binary(), User2 :: binary()}, state()) -> {true | false, state()}.
auth({_User1, User2}, #state{db=Db}=State) ->
    case lists:member(User2, Db) of
        true -> {true, State};
        false -> {false, State}
    end.

-spec share_group(User1 :: binary(), User2 :: binary(), state()) -> {true | false, state()}.
share_group(User, User, State) ->
    {true, State};
share_group(_User1, User2, State) ->
    Groups = element(2, User2),		
    if 
        is_binary(Groups) == true -> {true, State};
        Groups == undefined -> {false, State}
    end.

-spec create_group(Group :: term(), state()) -> {true | false, state()}.
create_group(Group, #state{lists_group = Lgroup}=State) ->
    Lgroup1 = Lgroup++Group,
    {true, State#state{lists_group=Lgroup1}}.

-spec delete_group(Group :: term(), state()) -> {true | false, state()}.
delete_group(_Group, State) ->
    {true, State}.

-spec create_ingroup(User :: term(), Group :: term(), state()) -> {true | false, state()}.
create_ingroup(User, Group, #state{table = Table, lists_group = Lgroup, session=Session}=State) ->
    case ets:member(Table, User) of            
        true -> case lists:member(Group, Lgroup) of
                    true -> 
                        LNick = ets:lookup(Table, User),
                        TNick = lists:last(LNick),
                        Nick = element(2, TNick),
                        Iq = exmpp_client_roster:set_item(User, [Group], Nick),
                        exmpp_session:send_packet(Session, Iq),
                        ets:insert(Table, {User, Group}),
                        {true, State};
                    false -> {false, State}
                end;
        false -> {false, State}
    end.

-spec delete_ingroup(InGroup :: term(), state()) -> {true | false, state()}.
delete_ingroup(_InGroup, State) ->
    {true, State}.

-spec update_group(Group :: term(), state()) -> {true | false, state()}.
update_group(_Group, State) ->
    {true, State}.

-spec create_user(Jid :: term(), User :: term(), state()) -> {true | false, state()}.
create_user(Jid, User, #state{table = Table}=State) ->
    ets:insert(Table, {Jid, User}),
    {true, State}.

-spec delete_user(User :: term(), state()) -> {true | false, state()}.
delete_user(_User, State) ->
    {true, State}.

-spec get_groups(state()) -> {term(), state()}.
get_groups(#state{lists_group=Lgroup}=State) ->
    {Lgroup, State}.