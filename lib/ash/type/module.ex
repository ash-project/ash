defmodule Ash.Type.Module do
  @constraints [
    behaviour: [
      type: :atom,
      doc: "Allows constraining the module a one which implements a behaviour"
    ],
    protocol: [
      type: :atom,
      doc: "Allows constraining the module a one which implements a protocol"
    ]
  ]

  @moduledoc """
  Stores a module as a string in the database.

  A builtin type that can be referenced via `:module`.

  ### Constraints

  #{Spark.Options.docs(@constraints)}
  """
  use Ash.Type

  @impl true
  def storage_type(_), do: :string

  @impl true
  def constraints, do: @constraints

  @impl true
  def cast_atomic(new_value, _constraints) do
    {:atomic, new_value}
  end

  def apply_constraints(nil, _), do: :ok

  def apply_constraints(value, constraints) do
    []
    |> apply_behaviour_constraint(value, constraints[:behaviour])
    |> apply_protocol_constraint(value, constraints[:protocol])
    |> case do
      [] -> {:ok, value}
      errors -> {:error, errors}
    end
  end

  defp apply_behaviour_constraint(errors, _module, nil), do: errors

  defp apply_behaviour_constraint(errors, module, behaviour) do
    if Spark.implements_behaviour?(module, behaviour) do
      errors
    else
      Enum.concat(errors, [
        [
          message: "module %{module} does not implement the %{behaviour} behaviour",
          module: module,
          behaviour: behaviour
        ]
      ])
    end
  end

  defp apply_protocol_constraint(errors, _module, nil), do: errors

  defp apply_protocol_constraint(errors, module, protocol) do
    Protocol.assert_protocol!(protocol)
    Protocol.assert_impl!(protocol, module)

    errors
  rescue
    ArgumentError ->
      Enum.concat(errors, [
        [
          message: "module %{module} does not implement the %{protocol} protocol",
          module: module,
          protocol: protocol
        ]
      ])
  end

  @impl true
  def cast_input(value, _) when is_atom(value) do
    if Code.ensure_loaded?(value) do
      {:ok, value}
    else
      :error
    end
  end

  def cast_input("", _), do: {:ok, nil}

  def cast_input("Elixir." <> _ = value, _) do
    module = Module.concat([value])

    if Code.ensure_loaded?(module) do
      {:ok, module}
    else
      :error
    end
  end

  def cast_input(value, _) when is_binary(value) do
    atom = String.to_existing_atom(value)

    if Code.ensure_loaded?(atom) do
      {:ok, atom}
    else
      :error
    end
  rescue
    ArgumentError ->
      :error
  end

  def cast_input(_, _), do: :error

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, _) when is_atom(value) do
    if Code.ensure_loaded?(value) do
      {:ok, value}
    else
      :error
    end
  end

  def cast_stored("Elixir." <> _ = value, _) do
    module = Module.concat([value])

    if Code.ensure_loaded?(module) do
      {:ok, module}
    else
      :error
    end
  end

  def cast_stored(value, _) when is_binary(value) do
    atom = String.to_existing_atom(value)

    if Code.ensure_loaded?(atom) do
      {:ok, atom}
    else
      :error
    end
  rescue
    ArgumentError -> :error
  end

  def cast_stored(_, _), do: :error

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) when is_atom(value) do
    {:ok, to_string(value)}
  end

  def dump_to_native(_, _), do: :error
end
