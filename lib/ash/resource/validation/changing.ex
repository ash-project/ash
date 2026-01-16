# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Validation.Changing do
  @moduledoc false

  use Ash.Resource.Validation

  import Ash.Changeset

  alias Ash.Error.Changes.InvalidAttribute

  defmodule Opts do
    @moduledoc false

    use Spark.Options.Validator,
      schema: [
        field: [
          type: :atom,
          required: true,
          doc:
            "The attribute or relationship to check for changes. Using a relationship does not compare old and new value, returning `true` if the value is being touched)"
        ],
        touching?: [
          type: :atom,
          required: false,
          default: false,
          doc:
            "Whether to consider a field as changing if it is just being touched (i.e consider it changed even if it is being changed to its current value)"
        ],
        to: [
          type: :any,
          required: false,
          doc: "Only passes if the value is being changed to a given value"
        ],
        from: [
          type: :any,
          required: false,
          doc: "Only passes if the value is being changed from a given value"
        ]
      ]
  end

  @default_error_message "must be changing"

  @impl Ash.Resource.Validation
  def init(opts) do
    case Opts.validate(opts) do
      {:ok, opts} ->
        {:ok, Opts.to_options(opts)}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  @impl Ash.Resource.Validation
  def validate(changeset, opts, context) do
    if field_is_changing?(changeset, Keyword.fetch!(opts, :field), opts),
      do: :ok,
      else: {:error, exception(opts, context)}
  end

  defp field_is_changing?(changeset, field, opts) do
    touching? = opts[:touching?]

    case Ash.Resource.Info.relationship(changeset.resource, field) do
      nil ->
        attribute_matches_opts?(changeset, field, touching?, opts)

      %{type: :belongs_to} = relationship ->
        attribute_matches_opts?(changeset, relationship.source_attribute, touching?, opts) ||
          changing_relationship?(changeset, relationship.name)

      relationship ->
        if Keyword.has_key?(opts, :to) || Keyword.has_key?(opts, :from) do
          false
        else
          changing_relationship?(changeset, relationship.name)
        end
    end
  end

  defp attribute_matches_opts?(changeset, field, touching?, opts) do
    if attribute_is_changing?(changeset, field, touching?) do
      change_matches_values?(changeset, field, opts)
    else
      false
    end
  end

  defp attribute_is_changing?(changeset, field, true) do
    Map.has_key?(changeset.casted_attributes, field) ||
      Map.has_key?(changeset.attributes, field) || Keyword.has_key?(changeset.atomics, field)
  end

  defp attribute_is_changing?(changeset, field, _), do: changing_attribute?(changeset, field)

  defp change_matches_values?(changeset, field, opts) do
    from = Keyword.fetch(opts, :from)
    to = Keyword.fetch(opts, :to)

    cond do
      from == :error and to == :error ->
        true

      match?({:ok, _}, from) and changeset.action_type == :create ->
        false

      true ->
        from_value = maybe_get_from_value(changeset, field, from)
        to_value = maybe_get_to_value(changeset, field, to)

        case {from, to, from_value, to_value} do
          {{:ok, from_value}, {:ok, to_value}, {:ok, current}, {:ok, new}} ->
            current == from_value && new == to_value

          {{:ok, from_value}, :error, {:ok, current}, _} ->
            current == from_value

          {:error, {:ok, to_value}, _, {:ok, new}} ->
            new == to_value

          _ ->
            false
        end
    end
  end

  defp maybe_get_from_value(changeset, field, {:ok, _from}) do
    Ash.Changeset.fetch_data(changeset, field)
  end

  defp maybe_get_from_value(_changeset, _field, :error), do: {:ok, nil}

  defp maybe_get_to_value(changeset, field, {:ok, _to}) do
    fetch_new_value(changeset, field)
  end

  defp maybe_get_to_value(_changeset, _field, :error), do: {:ok, nil}

  defp fetch_new_value(changeset, field) do
    case Ash.Changeset.fetch_change(changeset, field) do
      {:ok, value} ->
        {:ok, value}

      :error ->
        case Map.fetch(changeset.casted_attributes, field) do
          {:ok, value} ->
            {:ok, value}

          :error ->
            Keyword.fetch(changeset.atomics, field)
        end
    end
  end

  @impl Ash.Resource.Validation
  def atomic(changeset, opts, context) do
    if opts[:touching?] do
      validate(changeset, opts, context)
    else
      if Keyword.has_key?(opts, :from) && changeset.action_type == :create do
        {:error, exception(opts, context)}
      else
        case atomic_field(changeset, Keyword.fetch!(opts, :field)) do
          {:changing, field} ->
            {:atomic, [field], atomic_condition(field, opts),
             expr(
               error(^InvalidAttribute, %{
                 field: ^field,
                 value: ^atomic_ref(field),
                 message: ^(context.message || @default_error_message),
                 vars: %{field: ^field}
               })
             )}

          {:not_changing, _field} ->
            {:error, exception(opts, context)}

          {:not_atomic, field} ->
            {:not_atomic, "can't atomically check if #{field} relationship is changing"}
        end
      end
    end
  end

  defp atomic_condition(field, opts) do
    case {Keyword.fetch(opts, :from), Keyword.fetch(opts, :to)} do
      {{:ok, from}, {:ok, to}} ->
        match_from = ref_matches_value(field, from)
        match_to = atomic_ref_matches_value(field, to)

        expr(not (^match_from and ^match_to))

      {{:ok, from}, :error} ->
        match_from = ref_matches_value(field, from)

        expr(not ^match_from)

      {:error, {:ok, to}} ->
        match_to = atomic_ref_matches_value(field, to)

        expr(not ^match_to)

      {:error, :error} ->
        expr(
          (is_nil(^atomic_ref(field)) and is_nil(^ref(field))) or
            (not is_nil(^ref(field)) and ^atomic_ref(field) == ^ref(field))
        )
    end
  end

  defp ref_matches_value(field, value) do
    if is_nil(value) do
      expr(is_nil(^ref(field)))
    else
      expr(^ref(field) == ^value)
    end
  end

  defp atomic_ref_matches_value(field, value) do
    if is_nil(value) do
      expr(is_nil(^atomic_ref(field)))
    else
      expr(^atomic_ref(field) == ^value)
    end
  end

  defp atomic_field(changeset, field) do
    changeset.resource
    |> Ash.Resource.Info.relationship(field)
    |> case do
      nil -> {:atomic, field}
      %{type: :belongs_to} = relationship -> {:atomic, relationship.source_attribute}
      _relationship -> {:not_atomic, field}
    end
    |> atomic_result(changeset)
  end

  defp atomic_result({:atomic, field}, changeset) do
    msg =
      if changing_attribute?(changeset, field),
        do: :changing,
        else: :not_changing

    {msg, field}
  end

  defp atomic_result(not_atomic, _changeset), do: not_atomic

  @impl Ash.Resource.Validation
  def describe(_opts), do: [message: @default_error_message, vars: []]

  defp exception(opts, context) do
    [field: opts[:field]]
    |> with_description(opts)
    |> maybe_use_context_message(context.message)
    |> InvalidAttribute.exception()
  end

  defp maybe_use_context_message(opts, nil), do: opts

  defp maybe_use_context_message(opts, message) do
    Keyword.put(opts, :message, message)
  end
end
