defmodule Ash.Resource.Validation.Present do
  @moduledoc false
  use Ash.Resource.Validation

  alias Ash.Error.Changes.{InvalidAttribute, InvalidChanges}

  @impl true
  def init(opts) do
    case Spark.OptionsHelpers.validate(
           opts,
           Keyword.put(Ash.Resource.Validation.Builtins.present_opts(), :attributes,
             type: {:custom, __MODULE__, :attributes, []},
             required: true
           )
         ) do
      {:ok, opts} ->
        {:ok, opts}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  @impl true
  def validate(changeset, opts) do
    {present, count} =
      Enum.reduce(opts[:attributes], {0, 0}, fn attribute, {present, count} ->
        if is_nil(Ash.Changeset.get_argument_or_attribute(changeset, attribute)) do
          {present, count + 1}
        else
          {present + 1, count + 1}
        end
      end)

    opts =
      opts
      |> Keyword.put(:keys, Enum.join(opts[:attributes] || [], ","))
      |> Keyword.put(:fields, opts[:attributes])

    cond do
      opts[:exactly] && present != opts[:exactly] ->
        if opts[:exactly] == 0 do
          changes_error(opts, count)
        else
          if count == 1 do
            attribute_error(changeset, opts, count)
          else
            attribute_error(
              changeset,
              opts,
              count
            )
          end
        end

      opts[:at_least] && present < opts[:at_least] ->
        if count == 1 do
          attribute_error(changeset, opts, count)
        else
          changes_error(opts, count)
        end

      opts[:at_most] && present > opts[:at_most] ->
        if count == 1 do
          attribute_error(changeset, opts, count)
        else
          changes_error(opts, count)
        end

      true ->
        :ok
    end
  end

  @impl true
  def describe(opts) do
    [vars: opts, message: message(opts)]
  end

  defp message(opts) do
    cond do
      opts[:exactly] == 0 -> "must be absent"
      length(opts[:attributes]) == 1 and not is_nil(opts[:at_most]) -> "must not be present"
      length(opts[:attributes]) == 1 -> "must be present"
      not is_nil(opts[:exactly]) -> "exactly %{exactly} of %{keys} must be present"
      not is_nil(opts[:at_least]) -> "at least %{at_least} of %{keys} must be present"
      not is_nil(opts[:at_most]) -> "at most %{at_most} of %{keys} must be present"
    end
  end

  defp changes_error(opts, _count) do
    {:error,
     [fields: opts[:attributes]]
     |> with_description(opts)
     |> InvalidChanges.exception()}
  end

  defp attribute_error(changeset, opts, _count) do
    {:error,
     opts[:attributes]
     |> List.wrap()
     |> Enum.map(fn attribute ->
       [
         field: attribute,
         value: Ash.Changeset.get_attribute(changeset, attribute)
       ]
       |> with_description(opts)
       |> InvalidAttribute.exception()
     end)}
  end

  @doc false
  def attributes(attributes) do
    attributes = List.wrap(attributes)

    if Enum.all?(attributes, &is_atom/1) do
      {:ok, attributes}
    else
      {:error, "Expected all attributes provided to be atoms."}
    end
  end
end
