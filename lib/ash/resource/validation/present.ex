defmodule Ash.Resource.Validation.Present do
  @moduledoc false
  use Ash.Resource.Validation

  alias Ash.Error.Changes.{InvalidAttribute, InvalidChanges}
  import Ash.Expr

  @opt_schema [
    at_least: [
      type: :non_neg_integer,
      doc: "At least this many must be present. Defaults to the number of attributes provided"
    ],
    at_most: [
      type: :non_neg_integer,
      doc: "At most this many must be present. Defaults to the number of attributes provided"
    ],
    exactly: [
      type: :non_neg_integer,
      doc: "Exactly this many must be present"
    ],
    attributes: [
      type: {:wrap_list, :atom},
      required: true,
      hide: true
    ]
  ]

  def opt_schema, do: @opt_schema

  opt_schema = @opt_schema

  defmodule Opts do
    @moduledoc false
    use Spark.Options.Validator, schema: opt_schema
  end

  @impl true
  def init(opts) do
    case Opts.validate(opts) do
      {:ok, opts} ->
        {:ok, Opts.to_options(opts)}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  @impl true
  def supports(_opts), do: [Ash.Changeset, Ash.Query, Ash.ActionInput]

  @impl true
  def validate(subject, opts, _context) do
    {present, count} =
      Enum.reduce(opts[:attributes], {0, 0}, fn attribute, {present, count} ->
        if present?(subject, attribute) do
          {present + 1, count + 1}
        else
          {present, count + 1}
        end
      end)

    do_validate(subject, opts, present, count)
  end

  def do_validate(subject, opts, present, count) do
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
            attribute_error(subject, opts, count)
          else
            changes_error(opts, count)
          end
        end

      opts[:at_least] && present < opts[:at_least] ->
        if count == 1 do
          attribute_error(subject, opts, count)
        else
          changes_error(opts, count)
        end

      opts[:at_most] && present > opts[:at_most] ->
        if count == 1 do
          attribute_error(subject, opts, count)
        else
          changes_error(opts, count)
        end

      true ->
        :ok
    end
  end

  @impl true
  def atomic(changeset, opts, context) do
    values =
      Enum.map(opts[:attributes], fn attr ->
        if Enum.any?(changeset.action.arguments, &(&1.name == attr)) do
          Ash.Changeset.get_argument(changeset, attr)
        else
          expr(^atomic_ref(attr))
        end
      end)

    atomic_for_values(opts, context, values)
  end

  def atomic_for_values(opts, context, values) do
    attribute_count = length(opts[:attributes])
    nil_count = expr(count_nils(^values))

    opts
    |> Keyword.delete(:attributes)
    |> Enum.map(fn
      {:exactly, exactly} ->
        message =
          cond do
            context.message ->
              context.message

            exactly == 0 ->
              "must be absent"

            attribute_count == 1 ->
              "must be present"

            true ->
              "exactly %{exactly} of %{keys} must be present"
          end

        if attribute_count == 1 do
          attribute = Enum.at(opts[:attributes], 0)

          condition =
            if exactly == 0 do
              expr(not is_nil(^atomic_ref(attribute)))
            else
              expr(is_nil(^atomic_ref(attribute)))
            end

          {:atomic, opts[:attributes], condition,
           expr(
             error(^InvalidAttribute, %{
               field: ^Enum.at(opts[:attributes], 0),
               value: ^atomic_ref(Enum.at(opts[:attributes], 0)),
               message: ^message,
               vars: %{exactly: ^exactly, keys: ^Enum.join(opts[:attributes], ", ")}
             })
           )}
        else
          exactly_nil = attribute_count - exactly

          {:atomic, opts[:attributes], expr(^nil_count != ^exactly_nil),
           expr(
             error(^InvalidAttribute, %{
               field: ^Enum.at(opts[:attributes], 0),
               value: ^atomic_ref(Enum.at(opts[:attributes], 0)),
               message: ^message,
               vars: %{exactly: ^exactly, keys: ^Enum.join(opts[:attributes], ", ")}
             })
           )}
        end

      {:at_least, at_least} ->
        at_most_nil = attribute_count - at_least

        {:atomic, opts[:attributes], expr(^nil_count > ^at_most_nil),
         expr(
           error(^InvalidAttribute, %{
             field: ^Enum.at(opts[:attributes], 0),
             value: ^atomic_ref(Enum.at(opts[:attributes], 0)),
             message: "at least %{at_least} of %{keys} must be present",
             vars: %{at_least: ^at_least, keys: ^Enum.join(opts[:attributes], ", ")}
           })
         )}

      {:at_most, at_most} ->
        at_least_nil = attribute_count - at_most

        {:atomic, opts[:attributes], expr(^nil_count < ^at_least_nil),
         expr(
           error(^InvalidAttribute, %{
             field: ^Enum.at(opts[:attributes], 0),
             value: ^atomic_ref(Enum.at(opts[:attributes], 0)),
             message: "at most %{at_most} of %{keys} must be present",
             vars: %{at_most: ^at_most, keys: ^Enum.join(opts[:attributes], ", ")}
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

  defp attribute_error(subject, opts, _count) do
    {:error,
     opts[:attributes]
     |> List.wrap()
     |> Enum.map(fn attribute ->
       [
         field: attribute,
         value: Ash.Subject.get_attribute(subject, attribute)
       ]
       |> with_description(opts)
       |> InvalidAttribute.exception()
     end)}
  end

  defp present?(%Ash.Changeset{} = changeset, attribute) do
    Ash.Changeset.present?(changeset, attribute)
  end

  defp present?(subject, attribute) do
    case Ash.Subject.get_argument(subject, attribute) do
      nil -> false
      _value -> true
    end
  end
end
