defmodule Ash.Type.Function do
  @moduledoc """
  Represents a function.

  If the type would be dumped to a native format, `:erlang.term_to_binary(term, [:safe])` is used.

  Please keep in mind, this is *NOT SAFE* to use with external input.

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
  def storage_type(_), do: :binary

  @impl true
  def constraints, do: @constraints

  @impl true
  def apply_constraints(term, constraints) do
    if constraints[:arity] && not is_function(term, constraints[:arity]) do
      {:error, message: "Expected a function of arity %{arity}", arity: constraints[:arity]}
    else
      {:ok, term}
    end
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
