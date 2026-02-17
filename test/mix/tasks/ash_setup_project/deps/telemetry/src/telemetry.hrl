-record(handler, {id :: telemetry:handler_id() | '_',
                  event_name :: telemetry:event_name() | '_',
                  function   :: telemetry:handler_function() | '_',
                  config     :: telemetry:handler_config() | '_'}).

-ifdef('OTP_RELEASE').
-define(WITH_STACKTRACE(T, R, S), T:R:S ->).
-else.
-define(WITH_STACKTRACE(T, R, S), T:R -> S = erlang:get_stacktrace(),).
-endif.

-ifdef('OTP_RELEASE').
-include_lib("kernel/include/logger.hrl").
-else.
-define(LOG_ERROR(Msg, Args), error_logger:error_msg(Msg, Args)).
-endif.

-ifdef('OTP_RELEASE').
-include_lib("kernel/include/logger.hrl").
-else.
-define(LOG_WARNING(Msg, Args), error_logger:warning_msg(Msg, Args)).
-endif.

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.