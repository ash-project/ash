defmodule Ash.Seed do
  @moduledoc """
  Helpers for seeding data, useful for quickly creating lots of data either for database seeding or testing.

  Important: this bypasses resource actions, and goes straight to the data layer. No action changes or validations are run.
  The only thing that it does at the moment is ensure that default values for attributes are set, it does not validate
  that required attributes are set (although the data layer may do that for you, e.g with ash_postgres).
  """

  @doc """
  Seed using a record (instance of a resource) as input.

  If the passed in struct was retrieved from the data layer already (i.e already seeded),
  then it is returned and nothing is done. Otherwise, the attributes and relationships are
  used as input to `seed/2`, after having any `%Ash.NotLoaded{}` values stripped out.

  Any `nil` values will be overwritten with their default values. To avoid this, either use `seed/2`
  in which providing the key will have it not set the default values.
  If you want to force `nil` to be accepted and prevent the default value from being set, use the
  `keep_nil/0` function provided here, which returns `:__keep_nil__`. Alternatively, use
  `seed!(Post, %{text: nil})`.

  See `seed!/2` for more information.
  """
  def seed!(%{__meta__: %{state: :loaded}} = input) do
    input
  end

  def seed!(%resource{} = input) do
    keys =
      Ash.Resource.Info.attributes(resource)
      |> Enum.concat(Ash.Resource.Info.relationships(resource))
      |> Enum.map(& &1.name)

    input =
      input
      |> Map.take(keys)
      |> Enum.reduce(%{}, fn
        {_, %Ash.NotLoaded{}}, acc ->
          acc

        {_, nil}, acc ->
          acc

        {key, :__keep_nil__}, acc ->
          Map.put(acc, key, nil)

        {key, value}, acc ->
          Map.put(acc, key, value)
      end)

    seed!(
      resource,
      input
    )
  end

  def seed!(records) when is_list(records) do
    Enum.map(records, &seed!/1)
  end

  @doc """
  Performs a direct call to the data layer of a resource with the provided input.

  If a list is provided as input, then you will get back that many results.
  """
  def seed!(resource, input) when is_list(input) do
    Enum.map(input, &seed!(resource, &1))
  end

  def seed!(resource, %resource{} = input) do
    seed!(input)
  end

  def seed!(resource, %other{}) do
    raise "Cannot seed #{inspect(resource)} with an input of type #{inspect(other)}"
  end

  def seed!(resource, input) when is_map(input) do
    resource
    |> Ash.Changeset.new()
    |> change_attributes(input)
    |> change_relationships(input)
    |> Ash.Changeset.set_defaults(:create, true)
    |> create_via_data_layer()
    |> case do
      {:ok, result, _, _} ->
        result

      {:error, error} ->
        raise Ash.Error.to_error_class(error)
    end
  end

  @doc """
  Returns `:__keep_nil__`, allowing to ensure a default value is not used when you want the value to be `nil`.
  """
  def keep_nil, do: :__keep_nil__

  defp create_via_data_layer(changeset) do
    Ash.Changeset.with_hooks(changeset, fn changeset ->
      Ash.DataLayer.create(changeset.resource, changeset)
    end)
  end

  defp change_attributes(changeset, input) do
    Enum.reduce(input, changeset, fn {key, value}, changeset ->
      case Ash.Resource.Info.attribute(changeset.resource, key) do
        nil ->
          changeset

        attribute ->
          Ash.Changeset.force_change_attribute(changeset, attribute.name, value)
      end
    end)
  end

  defp change_relationships(changeset, input) do
    Enum.reduce(input, changeset, fn {key, value}, changeset ->
      case Ash.Resource.Info.relationship(changeset.resource, key) do
        nil ->
          changeset

        %{
          type: :belongs_to,
          source_attribute: source_attribute,
          destination_attribute: destination_attribute,
          destination: destination,
          name: name
        } ->
          Ash.Changeset.around_action(changeset, fn changeset, callback ->
            related = seed!(destination, value)

            changeset
            |> Ash.Changeset.force_change_attribute(
              source_attribute,
              Map.get(related, destination_attribute)
            )
            |> callback.()
            |> case do
              {:ok, result, changeset, instructions} ->
                {:ok, Map.put(result, name, related), changeset, instructions}

              {:error, error} ->
                {:error, error}
            end
          end)

        %{
          type: :has_many,
          source_attribute: source_attribute,
          destination_attribute: destination_attribute,
          destination: destination,
          name: name
        } ->
          Ash.Changeset.after_action(changeset, fn _changeset, result ->
            related =
              value
              |> List.wrap()
              |> Enum.map(
                &update_or_seed!(
                  &1,
                  destination,
                  Map.get(result, source_attribute),
                  destination_attribute
                )
              )

            {:ok, Map.put(result, name, related)}
          end)

        %{
          type: :has_one,
          source_attribute: source_attribute,
          destination_attribute: destination_attribute,
          destination: destination,
          name: name
        } ->
          Ash.Changeset.after_action(changeset, fn _changeset, result ->
            if value do
              related =
                update_or_seed!(
                  value,
                  destination,
                  Map.get(result, source_attribute),
                  destination_attribute
                )

              {:ok, Map.put(result, name, related)}
            else
              {:ok, Map.put(result, name, nil)}
            end
          end)

        %{
          type: :many_to_many,
          source_attribute: source_attribute,
          source_attribute_on_join_resource: source_attribute_on_join_resource,
          destination_attribute_on_join_resource: destination_attribute_on_join_resource,
          join_relationship: join_relationship,
          destination_attribute: destination_attribute,
          destination: destination,
          through: through,
          name: name
        } ->
          Ash.Changeset.after_action(changeset, fn _changeset, result ->
            related = seed!(destination, List.wrap(value))

            through =
              Enum.map(related, fn related ->
                seed!(through, %{
                  source_attribute_on_join_resource => Map.get(result, source_attribute),
                  destination_attribute_on_join_resource =>
                    Map.get(related, destination_attribute)
                })
              end)

            {:ok, Map.merge(result, %{name => related, join_relationship => through})}
          end)
      end
    end)
  end

  defp update_or_seed!(
         %resource{__meta__: %{state: :loaded}} = record,
         resource,
         field_value,
         field
       ) do
    record = seed!(record)

    changeset =
      record
      |> Ash.Changeset.new()
      |> Ash.Changeset.force_change_attribute(field, field_value)

    case Ash.DataLayer.update(resource, changeset) do
      {:ok, result} ->
        result

      {:error, error} ->
        raise Ash.Error.to_error_class(error)
    end
  end

  defp update_or_seed!(input, resource, field_value, field) do
    seed!(
      resource,
      Map.put(input, field, field_value)
    )
  end
end
