defmodule Ash.Type.Function do
  @moduledoc """
  Represents a function.

  If the type would be dumped to a native format, `:erlang.term_to_binary(term, [:safe])` is used.

  More information available here: https://erlang.org/doc/man/erlang.html#binary_to_term-2
  """

  use Ash.Type

  @constraints [
    arity: [
      type: :pos_integer,
      doc: "Enforces a specific arity on the provided function"
    ]
  ]

  @impl true
  def storage_type, do: :binary

  @impl true
  def constraints, do: @constraints

  @impl true
  def apply_constraints(term, constraints) do
    if constraints[:arity] && not is_function(term, constraints[:arity]) do
      {:error, message: "Expected a function of arity %{arity}", arity: constraints[:arity]}
    else
      :ok
    end
  end

  @impl true
  def cast_input(value) when is_function(value) do
    {:ok, value}
  end

  def cast_input(_), do: :error

  @impl true
  def cast_stored(value) do
    case Ecto.Type.load(:binary, value) do
      {:ok, val} ->
        Ash.non_executable_binary_to_term(val, [:safe])

      other ->
        other
    end
  rescue
    _ ->
      :error
  end

  @impl true
  def dump_to_native(value) do
    Ecto.Type.dump(:binary, :erlang.term_to_binary(value))
  end
end
