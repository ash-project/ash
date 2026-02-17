%-
% Copyright (c) 2012-2014 Yakaz
% Copyright (c) 2016-2022 Jean-Sébastien Pédron <jean-sebastien.pedron@dumbbell.fr>
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions
% are met:
% 1. Redistributions of source code must retain the above copyright
%    notice, this list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright
%    notice, this list of conditions and the following disclaimer in the
%    documentation and/or other materials provided with the distribution.
%
% THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
% ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
% SUCH DAMAGE.

-ifndef(yamerl_nodes_yamerl_extensions_hrl).
-define(yamerl_nodes_yamerl_extensions_hrl, true).

-include("yamerl_types.hrl").

%% CAUTION:
%% Records defined in this file have default values for all members.
%% Those default values are often bad values but this is needed so that
%% Erlang won't add "undefined" in our back to the allowed values in the
%% type specifications.

%% -------------------------------------------------------------------
%% Nodes specifications.
%% -------------------------------------------------------------------

%% IP address/range/netmask.
-record(yamerl_ip_addr, {
    module  = undefined          :: atom(),
    tag     = "!"                :: tag_uri(),
    pres    = []                 :: list(),
    address = {0, 0, 0, 0}       :: inet:ip_address()
  }).
-type yamerl_ip_addr()           :: #yamerl_ip_addr{}.
-type yamerl_simple_ip_addr()    :: inet:ip_address().

-record(yamerl_ip_netmask, {
    module  = undefined          :: atom(),
    tag     = "!"                :: tag_uri(),
    pres    = []                 :: list(),
    address = {0, 0, 0, 0}       :: inet:ip_address(),
    mask    = 1                  :: pos_integer()
  }).
-type yamerl_ip_netmask()        :: #yamerl_ip_netmask{}.
-type yamerl_simple_ip_netmask() :: {inet:ip_address(), pos_integer()}.

-record(yamerl_ip_range, {
    module  = undefined          :: atom(),
    tag     = "!"                :: tag_uri(),
    pres    = []                 :: list(),
    from    = {0, 0, 0, 0}       :: inet:ip_address(),
    to      = {0, 0, 0, 0}       :: inet:ip_address()
  }).
-type yamerl_ip_range()          :: #yamerl_ip_range{}.
-type yamerl_simple_ip_range()   :: {inet:ip_address(), inet:ip_address()}.

-endif.
