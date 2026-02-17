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

-ifndef(yamerl_errors_hrl).
-define(yamerl_errors_hrl, true).

-include("yamerl_types.hrl").
-include("yamerl_tokens.hrl").

%% -------------------------------------------------------------------
%% Errors and warnings.
%% -------------------------------------------------------------------

-record(yamerl_invalid_option, {
    type   = error :: error | warning,
    text           :: string() | undefined,

    option         :: term()
  }).

-record(yamerl_parsing_error, {
    type   = error :: error | warning,
    text           :: string() | undefined,

    line           :: position() | undefined,
    column         :: position() | undefined,
    name   = error :: atom(),
    token          :: yamerl_partial_token()
                    | undefined,
    extra  = []    :: [term()]
  }).

-type yamerl_error() :: #yamerl_invalid_option{}
                      | #yamerl_parsing_error{}.

-record(yamerl_exception, {
    errors = [] :: [yamerl_error()]
  }).
-type yamerl_exception() :: #yamerl_exception{}.

-endif.
