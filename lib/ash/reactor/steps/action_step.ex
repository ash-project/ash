defmodule Ash.Reactor.ActionStep do
  @moduledoc """
  The Reactor step which is used to execute generic actions.
  """
  use Reactor.Step
  import Ash.Reactor.StepUtils

  alias Ash.{ActionInput, DataLayer}

  @doc false
  @impl true
  def run(arguments, _context, options) do
    action_input_options =
      options
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])

    action_options =
      []
      |> maybe_set_kw(:authorize?, options[:authorize?])

    options[:resource]
    |> ActionInput.for_action(options[:action], arguments[:input], action_input_options)
    |> options[:domain].run_action(action_options)
  end

  @doc false
  @impl true
  def undo(record, arguments, _context, options) do
    action_input_options =
      options
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])

    action_options =
      []
      |> maybe_set_kw(:authorize?, options[:authorize?])

    inputs =
      arguments[:input]
      |> Map.put(:result_to_undo, record)

    options[:resource]
    |> ActionInput.for_action(options[:action], inputs, action_input_options)
    |> options[:domain].run_action(action_options)
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
