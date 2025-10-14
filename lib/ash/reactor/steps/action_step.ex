# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Reactor.ActionStep do
  @moduledoc """
  The Reactor step which is used to execute generic actions.
  """
  use Reactor.Step
  import Ash.Reactor.StepUtils

  alias Ash.{ActionInput, DataLayer}

  @doc false
  @impl true
  def run(arguments, context, options) do
    action_input_options =
      options
      |> maybe_set_kw(:authorize?, context[:authorize?])
      |> maybe_set_kw(:actor, context[:actor])
      |> maybe_set_kw(:tenant, context[:tenant])
      |> maybe_set_kw(:tracer, context[:tracer])
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])
      |> Keyword.take(Ash.ActionInput.Opts.schema() |> Keyword.keys())

    action_options =
      [domain: options[:domain]]
      |> maybe_set_kw(:authorize?, options[:authorize?])

    options[:resource]
    |> ActionInput.for_action(options[:action], arguments[:input], action_input_options)
    |> ActionInput.set_context(arguments[:context] || %{})
    |> Ash.run_action(action_options)
    |> case do
      :ok ->
        {:ok, :ok}

      other ->
        other
    end
  end

  @doc false
  @impl true
  def undo(record, arguments, context, options) do
    action_input_options =
      options
      |> maybe_set_kw(:authorize?, context[:authorize?])
      |> maybe_set_kw(:actor, context[:actor])
      |> maybe_set_kw(:tenant, context[:tenant])
      |> maybe_set_kw(:tracer, context[:tracer])
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])
      |> Keyword.take(Ash.ActionInput.Opts.schema() |> Keyword.keys())
      |> Keyword.drop([:resource])
      |> Keyword.update(
        :skip_unknown_inputs,
        [:input_to_undo],
        &[:input_to_undo | &1]
      )

    action_options =
      []
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:domain, options[:domain])

    inputs =
      %{}
      |> Map.put(:result_to_undo, record)
      |> Map.put(:input_to_undo, arguments[:input])

    options[:resource]
    |> ActionInput.for_action(options[:undo_action], inputs, action_input_options)
    |> ActionInput.set_context(arguments[:context] || %{})
    |> Ash.run_action(action_options)
    |> case do
      {:ok, _} ->
        :ok

      other ->
        other
    end
  end

  @doc false
  @impl true
  def can?(%{impl: {_, options}}, :undo) do
    case options[:undo] do
      :always -> true
      :never -> false
      :outside_transaction -> !DataLayer.in_transaction?(options[:resource])
    end
  end

  def can?(_, :compensate), do: false

  @doc false
  @impl true
  def async?(%{impl: {_, options}, async?: async}) do
    cond do
      DataLayer.in_transaction?(options[:resource]) ->
        false

      is_function(async, 1) ->
        async.(options)

      true ->
        async
    end
  end
end
