# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.Function do
  @constraints [
    mfa: [
      type: :boolean,
      default: false,
      doc: """
      Restricts the value to external captures (`&Module.function/arity`), refusing \
      anonymous functions and closures. Only external captures can be stored: a closure \
      captures its environment, which cannot be rehydrated in another VM.
      """
    ],
    arity: [
      type: :pos_integer,
      doc: "Enforces a specific arity on the provided function"
    ]
  ]
  @moduledoc """
  Represents a function.

  By default a function is dumped to a native format with
  `:erlang.term_to_binary(term, [:safe])`, and this is *NOT SAFE* to use with
  external input (see https://erlang.org/doc/man/erlang.html#binary_to_term-2).

  With the `mfa` constraint the value is restricted to external captures
  (`&Module.function/arity`) and stored as a portable capture string such as
  `"&Elixir.MyApp.callback/2"`, late-bound back to the function on read. This
  decouples the stored reference from the compiled implementation, so it survives
  across VMs and releases where a serialized closure would not.

  ### Constraints

  #{Spark.Options.docs(@constraints)}
  """

  use Ash.Type
  import Ash.Gettext

  @impl true
  def storage_type(_), do: :binary

  @impl true
  def constraints, do: @constraints

  @impl true
  def apply_constraints(nil, _), do: {:ok, nil}

  def apply_constraints(term, constraints) do
    cond do
      constraints[:mfa] && not external?(term) ->
        {:error,
         message:
           error_message(
             "anonymous functions and closures cannot be stored; use a &Module.function/arity capture"
           )}

      constraints[:arity] && not is_function(term, constraints[:arity]) ->
        {:error,
         message: error_message("Expected a function of arity %{arity}"),
         arity: constraints[:arity]}

      true ->
        {:ok, term}
    end
  end

  defp external?(term) when is_function(term) do
    Function.info(term, :type) == {:type, :external}
  end

  defp external?(_), do: false

  @impl true
  def matches_type?(v, _constraints) do
    is_function(v)
  end

  @impl true
  def cast_input(value, _) when is_function(value) do
    {:ok, value}
  end

  def cast_input(_, _), do: :error

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  # sobelow_skip ["Misc.BinToTerm"]
  def cast_stored(value, _) do
    case Ecto.Type.load(:binary, value) do
      {:ok, val} ->
        case :erlang.binary_to_term(val, [:safe]) do
          function when is_function(function) ->
            function

          _ ->
            :error
        end

      other ->
        other
    end
  rescue
    _ ->
      :error
  end

  @impl true

  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(:binary, :erlang.term_to_binary(value))
  end
end
