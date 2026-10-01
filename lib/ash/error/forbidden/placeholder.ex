# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Forbidden.Placeholder do
  @moduledoc "A placeholder exception that the user should never see"
  use Splode.Error, fields: [:authorizer], class: :forbidden

  # `Ash.Can` passes the authorizer as a string so that it survives data layers
  # that serialize the `error/2` input. Data layers that evaluate `error/2` in
  # Elixir build this exception from that string directly.
  def exception(opts) do
    opts =
      case opts[:authorizer] do
        authorizer when is_binary(authorizer) ->
          Keyword.put(opts, :authorizer, Module.concat([authorizer]))

        _ ->
          opts
      end

    super(opts)
  end

  def from_json(%{"authorizer" => authorizer}) do
    exception(authorizer: authorizer)
  end

  def message(%{authorizer: authorizer}) do
    "This is a placeholder error that should be replaced for authorizer `#{inspect(authorizer)}` automatically. If you get it, please report a bug."
  end
end
