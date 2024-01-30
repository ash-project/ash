defmodule Ash.Resource.Validation.Present do
  @moduledoc false
  use Ash.Resource.Validation

  alias Ash.Error.Changes.{InvalidAttribute, InvalidChanges}
  import Ash.Filter.TemplateHelpers

  @impl true
  def init(opts) do
    case Spark.OptionsHelpers.validate(
           opts,
           Keyword.put(Ash.Resource.Validation.Builtins.present_opts(), :attributes,
             type: {:wrap_list, :atom},
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
        if Ash.Changeset.present?(changeset, attribute) do
          {present + 1, count + 1}
        else
          {present, count + 1}
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
  def atomic(changeset, opts) do
    values =
      Enum.map(opts[:attributes], fn attr ->
        case Ash.Changeset.fetch_argument(changeset, attr) do
          {:ok, value} ->
            value

          :error ->
            expr(^atomic_ref(attr))
        end
      end)

    nil_count = expr(count_nils(^values))

    opts
    |> Keyword.delete(:attributes)
    |> Enum.map(fn
      {:exactly, exactly} ->
        message =
          cond do
            exactly == 0 -> "must be absent"
            length(opts[:attributes]) == 1 -> "must be present"
            true -> "exactly %{exactly} of %{keys} must be present"
          end

        {:atomic, [opts[:attribute]], expr(^nil_count == ^exactly),
         expr(
           error(^InvalidAttribute, %{
             field: ^opts[:attribute],
             value: ^atomic_ref(opts[:attribute]),
             message: ^message,
             vars: %{exactly: ^exactly, keys: ^values}
           })
         )}

      {:at_least, at_least} ->
        {:atomic, [opts[:attribute]], expr(count_nils(^atomic_ref(opts[:attribute])) < ^at_least),
         expr(
           error(^InvalidAttribute, %{
             field: ^opts[:attribute],
             value: ^atomic_ref(opts[:attribute]),
             message: "at least %{at_least} of %{keys} must be present",
             vars: %{at_least: ^at_least, keys: ^values}
           })
         )}

      {:at_most, at_most} ->
        {:atomic, [opts[:attribute]], expr(count_nils(^atomic_ref(opts[:attribute])) > ^at_most),
         expr(
           error(^InvalidAttribute, %{
             field: ^opts[:attribute],
             value: ^atomic_ref(opts[:attribute]),
             message: "at most %{at_most} of %{keys} must be present",
             vars: %{at_most: ^at_most, keys: ^values}
           })
         )}
    end)
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
end
