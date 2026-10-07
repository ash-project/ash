# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.CodeInterface.Define do
  @moduledoc false
  # Helpers for `Ash.CodeInterface.__define_interface__/4`, which defines code
  # interface functions from compiled code rather than from quoted code that would
  # be interpreted in the module being compiled.

  @doc false
  # Defines the quoted `def` with the line of the `define` that declared it, so its
  # debug info points editors at that line. The file and the body's lines stay
  # `Ash.CodeInterface`'s (via `location: :keep`), so stacktraces are unchanged.
  def define_at(interface, env, {:def, meta, [call | body]}) do
    body =
      case body do
        [[do: body]] -> body
        [] -> nil
      end

    Ash.CodeInterface.eval_definition(call, body, Keyword.delete(meta, :line), interface, env)
  end

  @doc false
  # Equivalent of `@doc value` in the module being compiled.
  def put_doc(env, value), do: Module.put_attribute(env.module, :doc, {env.line, value})

  @doc false
  # Equivalent of `@dialyzer value` in the module being compiled.
  def put_dialyzer(env, value), do: Module.put_attribute(env.module, :dialyzer, value)
end
