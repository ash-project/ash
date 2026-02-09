# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Mix.Tasks.Install.PostgresCheck do
  @moduledoc false

  # Wraps :gen_tcp so the install task's Postgres check can be stubbed in tests.
  # :gen_tcp is a preloaded Erlang module and cannot be copied/stubbed by Mimic.

  def connect(host, port, opts, timeout) do
    :gen_tcp.connect(host, port, opts, timeout)
  end

  def close(socket) do
    :gen_tcp.close(socket)
  end
end
