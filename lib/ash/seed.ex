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

    seed!(resource, input)
  end

  def seed!(records) when is_list(records) do
    Enum.map(records, &seed!/1)
  end

  @doc """
  Performs a direct call to the data layer of a resource with the provided input.

  If a list is provided as input, then you will get back that many results.

  To set a tenant, use the tenant option.
  """

  def seed!(resource, input, opts \\ [])

  def seed!(resource, input, opts) when is_list(input) do
    # TODO: This should be implemented with bulk data layer callbacks
    Enum.map(input, &seed!(resource, &1, opts))
  end

  def seed!(resource, %resource{} = input, _opts) do
    seed!(input)
  end

  def seed!(resource, %other{}, _opts) do
    raise "Cannot seed #{inspect(resource)} with an input of type #{inspect(other)}"
  end

  def seed!(%resource{} = record, input, opts) when is_map(input) do
    attrs =
      resource
      |> Ash.Resource.Info.attributes()
      |> Enum.map(& &1.name)
      |> Enum.reject(&is_nil(Map.get(record, &1)))

    attr_input =
      record
      |> Map.take(attrs)
      |> Map.new(fn {key, value} ->
        if value == :__keep_nil__ do
          {key, nil}
        else
          {key, value}
        end
      end)

    resource
    |> Ash.Changeset.new()
    |> change_attributes(attr_input)
    |> change_attributes(input)
    |> change_relationships(input)
    |> Ash.Changeset.set_defaults(:create, true)
    |> Ash.Changeset.set_tenant(opts[:tenant])
    |> maybe_set_attribute_tenant()
    |> create_via_data_layer()
    |> case do
      {:ok, result, _, _} ->
        result

      {:error, error} ->
        raise Ash.Error.to_error_class(error)
    end
  end

  def seed!(resource, input, opts) when is_map(input) do
    attr_input =
      input
      |> Map.new(fn {key, value} ->
        if value == :__keep_nil__ do
          {key, nil}
        else
          {key, value}
        end
      end)

    resource
    |> Ash.Changeset.new()
    |> change_attributes(attr_input)
    |> change_relationships(input)
    |> Ash.Changeset.set_defaults(:create, true)
    |> Ash.Changeset.set_tenant(opts[:tenant])
    |> maybe_set_attribute_tenant()
    |> create_via_data_layer()
    |> case do
      {:ok, result, _, _} ->
        result

      {:error, error} ->
        raise Ash.Error.to_error_class(error)
    end
  end

  @doc """
  Performs an upsert operation on the data layer of a resource with the provided input and identities.
  The usage is the same as `seed!/1`, but it will update the record if it already exists.

  ```elixir
  Ash.Seed.upsert!(%User{email: 'test@gmail.com', name: 'Test'}, identity: :email)
  ```
  """
  def upsert!(_, opts \\ [])

  def upsert!(%{__meta__: %{state: :loaded}} = input, _otps) do
    input
  end

  def upsert!(%resource{} = input, opts) do
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

    upsert!(
      resource,
      input,
      opts
    )
  end

  def upsert!(records, opts) when is_list(records) do
    Enum.map(records, &upsert!(&1, opts))
  end

  def upsert!(resource, input) when is_atom(resource) and is_map(input) do
    upsert!(resource, input, [])
  end

  @doc """
  Usage is the same as `seed!/2`, but it will update the record if it already exists based on the identities.
  """
  def upsert!(resource, input, opts)

  def upsert!(resource, input, opts) when is_list(input) do
    # TODO: This should be implemented with bulk data layer callbacks
    Enum.map(input, &upsert!(resource, &1, opts))
  end

  def upsert!(resource, %resource{} = input, opts) do
    upsert!(input, opts)
  end

  def upsert!(resource, %other{}, _opts) do
    raise "Cannot upsert #{inspect(resource)} with an input of type #{inspect(other)}"
  end

  def upsert!(%resource{} = record, input, opts) when is_map(input) do
    attrs =
      resource
      |> Ash.Resource.Info.attributes()
      |> Enum.map(& &1.name)
      |> Enum.reject(&is_nil(Map.get(record, &1)))

    attr_input =
      record
      |> Map.take(attrs)
      |> Map.new(fn {key, value} ->
        if value == :__keep_nil__ do
          {key, nil}
        else
          {key, value}
        end
      end)

    resource
    |> Ash.Changeset.new()
    |> change_attributes(attr_input)
    |> change_attributes(input)
    |> change_relationships(input)
    |> Ash.Changeset.set_defaults(:create, true)
    |> Ash.Changeset.set_tenant(opts[:tenant])
    |> maybe_set_attribute_tenant()
    |> upsert_via_data_layer(opts[:identity])
    |> case do
      {:ok, result, _, _} ->
        result

      {:error, error} ->
        raise Ash.Error.to_error_class(error)
    end
  end

  def upsert!(resource, input, opts) when is_map(input) do
    attr_input =
      input
      |> Map.new(fn {key, value} ->
        if value == :__keep_nil__ do
          {key, nil}
        else
          {key, value}
        end
      end)

    resource
    |> Ash.Changeset.new()
    |> change_attributes(attr_input)
    |> change_relationships(input)
    |> Ash.Changeset.set_defaults(:create, true)
    |> Ash.Changeset.set_tenant(opts[:tenant])
    |> maybe_set_attribute_tenant()
    |> upsert_via_data_layer(opts[:identity])
    |> case do
      {:ok, result, _, _} ->
        result

      {:error, error} ->
        raise Ash.Error.to_error_class(error)
    end
  end

  def update!(record, input) when is_map(input) do
    record
    |> Ash.Changeset.new()
    |> change_attributes(input)
    |> change_relationships(input)
    |> Ash.Changeset.set_defaults(:update, true)
    |> update_via_data_layer()
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
      Ash.DataLayer.create(changeset.resource, Ash.Changeset.set_action_select(changeset))
    end)
  end

  defp upsert_via_data_layer(changeset, nil) do
    fields = Ash.Resource.Info.primary_key(changeset.resource)

    Ash.Changeset.with_hooks(changeset, fn changeset ->
      Ash.DataLayer.upsert(changeset.resource, Ash.Changeset.set_action_select(changeset), fields)
    end)
  end

  defp upsert_via_data_layer(changeset, identity) do
    fields = Ash.Resource.Info.identity(changeset.resource, identity).keys

    Ash.Changeset.with_hooks(changeset, fn changeset ->
      Ash.DataLayer.upsert(changeset.resource, Ash.Changeset.set_action_select(changeset), fields)
    end)
  end

  defp update_via_data_layer(changeset) do
    Ash.Changeset.with_hooks(changeset, fn changeset ->
      Ash.DataLayer.update(changeset.resource, Ash.Changeset.set_action_select(changeset))
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
      |> Ash.Changeset.set_action_select()

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

  defp maybe_set_attribute_tenant(changeset) do
    if changeset.tenant &&
         Ash.Resource.Info.multitenancy_strategy(changeset.resource) == :attribute do
      attribute = Ash.Resource.Info.multitenancy_attribute(changeset.resource)
      {m, f, a} = Ash.Resource.Info.multitenancy_parse_attribute(changeset.resource)
      attribute_value = apply(m, f, [changeset.to_tenant | a])

      Ash.Changeset.force_change_attribute(changeset, attribute, attribute_value)
    else
      changeset
    end
  end
end
