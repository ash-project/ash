defmodule Ash.Resource.Change.SetAttribute do
  @moduledoc false
  use Ash.Resource.Change
  alias Ash.Changeset

  def init(opts) do
    case Spark.OptionsHelpers.validate(opts, Ash.Resource.Change.Builtins.set_attribute_opts()) do
      {:ok, opts} ->
        {:ok, opts}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  def validate_value(value) when is_function(value, 0), do: {:ok, value}

  def validate_value(value) when is_function(value, 1), do: {:ok, value}

  def validate_value(value) when is_function(value),
    do: {:error, "only 0 argument functions are supported"}

  def validate_value(value), do: {:ok, value}

  def change(changeset, opts, _) do
    value =
      case opts[:value] do
        value when is_function(value, 0) ->
          value.()

        value when is_function(value, 1) ->
          value.(Changeset.get_attribute(changeset, opts[:attribute]))

        value ->
          value
      end

    if opts[:new?] do
      if Ash.Changeset.changing_attribute?(changeset, opts[:attribute]) do
        changeset
      else
        Changeset.force_change_attribute(changeset, opts[:attribute], value)
      end
    else
      if opts[:set_when_nil?] or
           Changeset.get_attribute(changeset, opts[:attribute]) != nil do
        Changeset.force_change_attribute(changeset, opts[:attribute], value)
      else
        changeset
      end
    end
  end
end
