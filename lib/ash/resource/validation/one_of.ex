defmodule Ash.Resource.Validation.OneOf do
  @moduledoc "Validates that the provided attribute is in a list of values"
  alias Ash.Error.Changes.InvalidAttribute

  @behaviour Ash.Resource.Validation

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
    case NimbleOptions.validate(opts, @opt_schema) do
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
        if changing_to in opts[:values] do
          :ok
        else
          {:error,
           InvalidAttribute.exception(
             field: opts[:attribute],
             validation: {:one_of, opts[:values]},
             message: "Expected value to be one of #{inspect(opts[:values])}"
           )}
        end

      _ ->
        :ok
    end
  end
end
