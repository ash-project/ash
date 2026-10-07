# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Actions.RunReactor do
  @moduledoc false
  use Ash.Resource.Actions.Implementation

  @impl true
  def run(input, opts, context) do
    {reactor, run_opts} = Keyword.pop!(opts, :reactor)

    run_opts =
      if input.action.transaction?,
        do: Keyword.put(run_opts, :async?, false),
        else: run_opts

    context =
      context
      |> Ash.Context.to_opts()
      |> Map.new()

    arguments =
      Enum.reduce(reactor.reactor().inputs, input.arguments, fn reactor_input, arguments ->
        Map.put_new(arguments, reactor_input.name, nil)
      end)

    reactor
    |> Reactor.run(arguments, context, run_opts)
    |> case do
      {:ok, _v} when is_nil(input.action.returns) ->
        :ok

      {:error, %{splode: Reactor.Error, errors: errors}} ->
        {:error, errors}

      other ->
        other
    end
  end

  @doc false
  def reactor?(module) when is_atom(module) do
    module.spark_is() == Reactor
  rescue
    UndefinedFunctionError -> false
  end
end
