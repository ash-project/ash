defmodule Ash.Resource.Validation.OneOf do
  @moduledoc false

  use Ash.Resource.Validation

  alias Ash.Error.Changes.InvalidAttribute

  @opt_schema [
    values: [
      type: {:custom, __MODULE__, :values, []},
      required: true
    ],
    attribute: [
      type: :atom,
      required: true
    ]
  ]

  @doc false
  def values(value) when is_list(value), do: {:ok, value}
  def values(_), do: {:error, "Expected a list of values"}

  @impl true
  def init(opts) do
    case Ash.OptionsHelpers.validate(opts, @opt_schema) do
      {:ok, opts} ->
        {:ok, opts}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  @impl true
  def validate(changeset, opts) do
    case Ash.Changeset.fetch_change(changeset, opts[:attribute]) do
      {:ok, nil} ->
        :ok

      {:ok, changing_to} ->
        if Enum.any?(opts[:values], &Comp.equal?(&1, changing_to)) do
          :ok
        else
          {:error,
           InvalidAttribute.exception(
             field: opts[:attribute],
             message: "expected one of %{values}",
             vars: [values: Enum.map_join(opts[:values], ", ", &to_string/1)]
           )}
        end

      _ ->
        :ok
    end
  end
end
