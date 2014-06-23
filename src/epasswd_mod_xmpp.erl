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
		 auth/2, share_group/3]).

-record(state, {db, path}).
-type state() :: #state{}.

-spec init([]) -> state().
init([]) ->
	F = fun() ->
			mnesia:all_keys(occi_user)
		end,
	Db_Tuple = mnesia:transaction(F),
	Db = element(2, Db_Tuple),
	#state{db = Db}.

-spec auth({User1 :: binary(), User2 :: binary()}, state()) -> {true | false, state()}.
auth({_User1, User2}, #state{db=Db}=State) ->
	case lists:member(User2, Db) of
		true -> {false, State};
		false -> {true, State}
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
