defmodule Ash.Actions.ChangesetHelpers do
  alias Ash.Actions.PrimaryKeyHelpers

  @type before_change_callback :: (Ecto.Changeset.t() -> Ecto.Changeset.t())
  @type after_change_callback ::
          (Ecto.Changeset.t(), Ash.record() -> {:ok, Ash.record()} | {:error, Ash.error()})

  @spec before_change(Ecto.Changeset.t(), before_change_callback) :: Ecto.Changeset.t()
  def before_change(changeset, func) do
    Map.update(changeset, :__before_ash_changes__, [func], fn funcs ->
      [func | funcs]
    end)
  end

  @spec after_change(Ecto.Changeset.t(), after_change_callback) :: Ecto.Changeset.t()
  def after_change(changeset, func) do
    Map.update(changeset, :__after_ash_changes__, [func], fn funcs ->
      [func | funcs]
    end)
  end

  @spec run_before_changes(Ecto.Changeset.t()) :: Ecto.Changeset.t()
  def run_before_changes(%{__before_ash_changes__: hooks} = changeset) do
    Enum.reduce(hooks, changeset, fn
      hook, %Ecto.Changeset{valid?: true} = changeset ->
        case hook.(changeset) do
          :ok -> changeset
          %Ecto.Changeset{} = changeset -> changeset
        end

      _, %Ecto.Changeset{} = changeset ->
        changeset
    end)
  end

  def run_before_changes(changeset), do: changeset

  @spec run_after_changes(Ecto.Changeset.t(), Ash.record()) ::
          {:ok, Ash.record()} | {:error, Ash.error()}
  def run_after_changes(%{__after_ash_changes__: hooks} = changeset, result) do
    Enum.reduce(hooks, {:ok, result}, fn
      hook, {:ok, result} ->
        case hook.(changeset, result) do
          {:ok, result} -> {:ok, result}
          {:error, error} -> {:error, error}
        end

      _, {:error, error} ->
        {:error, error}
    end)
  end

  def run_after_changes(_changeset, result) do
    {:ok, result}
  end

  @spec prepare_relationship_changes(
          Ecto.Changeset.t(),
          Ash.resource(),
          map(),
          boolean,
          Ash.user()
        ) :: Ecto.Changeset.t()
  def prepare_relationship_changes(
        changeset,
        resource,
        relationships,
        authorize?,
        user
      ) do
    Enum.reduce(relationships, changeset, fn {relationship, value}, changeset ->
      with {:rel, rel} when not is_nil(rel) <- {:rel, Ash.relationship(resource, relationship)},
           {:ok, filter} <- primary_key_filter(rel, value) do
        case rel.type do
          :belongs_to ->
            belongs_to_assoc_update(changeset, rel, filter, authorize?, user)

          :has_one ->
            has_one_assoc_update(changeset, rel, filter, authorize?, user)

          :has_many ->
            has_many_assoc_update(changeset, rel, filter, authorize?, user)

          :many_to_many ->
            many_to_many_assoc_update(changeset, rel, filter, authorize?, user)
        end
      else
        {:rel, nil} ->
          Ecto.Changeset.add_error(changeset, relationship, "No such relationship")

        {:error, error} ->
          Ecto.Changeset.add_error(changeset, relationship, error)
      end
    end)
  end

  defp primary_key_filter(%{cardinality: :many, destination: destination}, value) do
    PrimaryKeyHelpers.values_to_primary_key_filters(destination, value)
  end

  defp primary_key_filter(%{destination: destination}, value) do
    PrimaryKeyHelpers.value_to_primary_key_filter(destination, value)
  end

  defp belongs_to_assoc_update(
         %{__ash_api__: api} = changeset,
         %{
           destination: destination,
           destination_field: destination_field,
           source_field: source_field
         } = relationship,
         filter,
         authorize?,
         user
       ) do
    before_change(changeset, fn changeset ->
      case api.get(destination, filter, authorize?: authorize?, user: user) do
        {:ok, record} when not is_nil(record) ->
          changeset
          |> Ecto.Changeset.put_change(source_field, Map.get(record, destination_field))
          |> after_change(fn _changeset, result ->
            {:ok, Map.put(result, relationship.name, record)}
          end)

        {:ok, nil} ->
          {:error, "not found"}

        {:error, error} ->
          {:error, error}
      end
    end)
  end

  defp has_one_assoc_update(
         %{__ash_api__: api} = changeset,
         %{
           destination: destination,
           destination_field: destination_field,
           source_field: source_field
         } = relationship,
         filter,
         authorize?,
         user
       ) do
    changeset
    |> before_change(fn changeset ->
      value = Map.get(changeset.data, source_field)

      if changeset.action == :update && value do
        case api.get(destination, [{destination_field, value}]) do
          {:ok, nil} ->
            changeset

          {:ok, record} ->
            case api.update(record, attributes: %{destination_field => nil}) do
              {:ok, _} ->
                changeset

              {:error, error} ->
                Ecto.Changeset.add_error(changeset, relationship.name, error)
            end

          {:error, error} ->
            Ecto.Changeset.add_error(
              changeset,
              relationship.name,
              error
            )
        end
      else
        changeset
      end
    end)
    |> after_change(fn _changeset, result ->
      value = Map.get(result, source_field)

      with {:ok, record} <-
             api.get(destination, filter, authorize?: authorize?, user: user),
           {:ok, updated_record} <-
             api.update(record, attributes: %{destination_field => value}) do
        {:ok, Map.put(result, relationship.name, updated_record)}
      end
    end)
  end

  defp many_to_many_assoc_update(changeset, %{name: rel_name}, filter, _, _)
       when not is_list(filter) do
    Ecto.Changeset.add_error(changeset, rel_name, "Invalid value")
  end

  defp many_to_many_assoc_update(changeset, rel, filters, authorize?, user) do
    changeset
    |> before_change(fn %{__ash_api__: api} = changeset ->
      source_field_value = Ecto.Changeset.get_field(changeset, rel.source_field)

      destroy_result =
        destroy_no_longer_related_join_table_rows(
          api,
          source_field_value,
          rel,
          filters,
          authorize?,
          user
        )

      case destroy_result do
        :ok -> changeset
        {:error, error} -> {:error, error}
      end
    end)
    |> after_change(fn %{__ash_api__: api}, result ->
      case fetch_and_ensure_related(filters, api, result, rel, authorize?, user) do
        {:error, error} ->
          {:error, error}

        {:ok, related} ->
          {:ok, Map.put(result, rel.name, related)}
      end
    end)
  end

  defp has_many_assoc_update(
         %{__ash_api__: api} = changeset,
         %{
           destination: destination,
           destination_field: destination_field,
           source_field: source_field
         } = relationship,
         filters,
         authorize?,
         user
       ) do
    after_change(changeset, fn _changeset, %resource{} = result ->
      value = Map.get(result, source_field)

      currently_related_filter =
        result
        |> Map.take(Ash.primary_key(resource))
        |> Map.to_list()

      unless relationship.reverse_relationship do
        raise "Require reverse relationship for #{inspect(relationship)}"
      end

      params = [
        filter: [{relationship.reverse_relationship, currently_related_filter}],
        paginate?: false,
        authorize?: authorize?,
        user: user
      ]

      with {:ok, %{results: related}} <-
             api.read(destination, params),
           {:ok, to_relate} <-
             get_to_relate(api, filters, destination, authorize?, user),
           to_clear <-
             get_no_longer_present(resource, related, to_relate),
           :ok <-
             clear_no_longer_related(api, resource, to_clear, destination_field, authorize?, user),
           {:ok, now_related} <-
             relate_items(api, to_relate, destination_field, value, authorize?, user) do
        {:ok, Map.put(result, relationship.name, now_related)}
      end
    end)
  end

  defp fetch_and_ensure_related(filters, api, %resource{} = result, rel, authorize?, user) do
    pkey_filter = result |> Map.take(Ash.primary_key(resource)) |> Map.to_list()

    case api.read(rel.destination,
           filter: [{rel.reverse_relationship, pkey_filter}],
           authorize?: authorize?,
           user: user
         ) do
      {:ok, %{results: currently_related}} ->
        Enum.reduce_while(filters, {:ok, []}, fn filter, {:ok, records} ->
          already_related =
            Enum.find_value(currently_related, fn related_item ->
              if Map.take(related_item, Ash.primary_key(rel.destination)) ==
                   Enum.into(filter, %{}) do
                related_item
              else
                nil
              end
            end)

          if already_related do
            {:ok, already_related}
          else
            case do_fetch_and_ensure_related(api, result, filter, rel, authorize?, user) do
              {:ok, related} -> {:cont, {:ok, [related | records]}}
              {:error, error} -> {:halt, {:error, error}}
            end
          end
        end)

      {:error, error} ->
        {:error, error}
    end
  end

  defp destroy_no_longer_related_join_table_rows(
         api,
         source_field_value,
         rel,
         identifiers,
         authorize?,
         user
       ) do
    unless rel.reverse_relationship do
      raise "Need reverse relationship for #{inspect(rel)}"
    end

    filter = [
      {rel.reverse_relationship, [{rel.source_field, source_field_value}]},
      {:not, [or: identifiers]}
    ]

    case api.read(rel.destination, filter: filter, authorize?: authorize?, user: user) do
      {:error, error} ->
        {:error, error}

      {:ok, %{results: results}} ->
        Enum.reduce_while(results, :ok, fn result, :ok ->
          destination_field_value = Map.get(result, rel.destination_field)

          with {:ok, record} when not is_nil(record) <-
                 api.get(rel.through, [
                   {rel.source_field_on_join_table, source_field_value},
                   {rel.destination_field_on_join_table, destination_field_value}
                 ]),
               {:ok, _} <- api.destroy(record, authorize?: authorize?, user: user) do
            {:cont, :ok}
          else
            {:ok, nil} -> {:cont, :ok}
            {:error, error} -> {:halt, {:error, error}}
          end
        end)
    end
  end

  defp do_fetch_and_ensure_related(api, result, id_filter, rel, authorize?, user) do
    case api.get(rel.destination, id_filter, authorize?: authorize?, user: user) do
      {:error, error} ->
        {:error, error}

      {:ok, record} ->
        destination_field_value = Map.get(record, rel.destination_field)
        source_field_value = Map.get(result, rel.source_field)

        filter = [
          {rel.source_field_on_join_table, [eq: source_field_value]},
          {rel.destination_field_on_join_table, [eq: destination_field_value]}
        ]

        # This is very unoptimized. Also, if `destination_key` is present in the identifier
        # we should consider just trying to insert with that. At some point we can validate
        # read authorization without running the read action and then there is no reason to read,
        # unless we feel that verifying at *runtime* that the record exists before-hand is a good
        # idea
        case api.get(rel.through, filter, authorize?: authorize?, user: user) do
          {:ok, nil} ->
            create_result =
              api.create(rel.through,
                authorize?: authorize?,
                user: user,
                attributes: %{
                  rel.destination_field_on_join_table => destination_field_value,
                  rel.source_field_on_join_table => source_field_value
                }
              )

            case create_result do
              {:ok, _} -> {:ok, record}
              {:error, error} -> {:error, error}
            end

          {:ok, _} ->
            {:ok, record}

          {:error, error} ->
            {:error, error}
        end
    end
  end

  defp relate_items(api, to_relate, destination_field, destination_field_value, authorize?, user) do
    Enum.reduce(to_relate, {:ok, []}, fn
      to_be_related, {:ok, now_related} ->
        case api.update(to_be_related,
               attributes: %{destination_field => destination_field_value},
               authorize?: authorize?,
               user: user
             ) do
          {:ok, newly_related} -> {:ok, [newly_related | now_related]}
          {:error, error} -> {:error, error}
        end

      _, {:error, error} ->
        {:error, error}
    end)
  end

  defp clear_no_longer_related(api, _resource, to_clear, destination_key, authorize?, user) do
    Enum.reduce(to_clear, :ok, fn
      record, :ok ->
        case api.update(record,
               attributes: %{destination_key => nil},
               authorize?: authorize?,
               user: user
             ) do
          {:ok, _} -> :ok
          {:error, error} -> {:error, error}
        end

      _record, {:error, error} ->
        {:error, error}
    end)
  end

  defp get_no_longer_present(resource, currently_related, to_relate) do
    primary_key = Ash.primary_key(resource)

    to_relate_pkeys =
      to_relate
      |> Enum.map(fn to_relate_item ->
        Map.take(to_relate_item, primary_key)
      end)
      |> MapSet.new()

    Enum.reject(currently_related, fn related_item ->
      MapSet.member?(to_relate_pkeys, Map.take(related_item, primary_key))
    end)
  end

  defp get_to_relate(api, filters, destination, authorize?, user) do
    # TODO: Only fetch the ones that we don't already have
    Enum.reduce(filters, {:ok, []}, fn
      filter, {:ok, to_relate} ->
        case api.get(destination, filter, authorize?: authorize?, user: user) do
          {:ok, to_relate_item} -> {:ok, [to_relate_item | to_relate]}
          {:error, errors} -> {:error, errors}
        end

      _, {:error, error} ->
        {:error, error}
    end)
  end
end
