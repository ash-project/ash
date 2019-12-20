defmodule Ash.Api.Interface do
  defmacro __using__(_) do
    quote do
      def get!(resource, id, params \\ []) do
        Ash.Api.Interface.get!(__MODULE__, resource, id, params)
      end

      def get(resource, id, params \\ []) do
        case Ash.Api.Interface.get(__MODULE__, resource, id, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      def read!(resource, params \\ []) do
        Ash.Api.Interface.read!(__MODULE__, resource, params)
      end

      def read(resource, params \\ []) do
        case Ash.Api.Interface.read(__MODULE__, resource, params) do
          {:ok, paginator} -> {:ok, paginator}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      def create!(resource, params \\ []) do
        Ash.Api.Interface.create!(__MODULE__, resource, params)
      end

      def create(resource, params \\ []) do
        case Ash.Api.Interface.create(__MODULE__, resource, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      def update!(record, params \\ []) do
        Ash.Api.Interface.update!(__MODULE__, record, params)
      end

      def update(record, params \\ []) do
        case Ash.Api.Interface.update(__MODULE__, record, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end
    end
  end

  def get!(api, resource, id, params \\ []) do
    api
    |> get(resource, id, params)
    |> unwrap_or_raise!()
  end

  def get(api, resource, filter, params) do
    case api.get_resource(resource) do
      {:ok, resource} ->
        primary_key = Ash.primary_key(resource)

        adjusted_filter =
          cond do
            Keyword.keyword?(filter) ->
              filter

            Enum.count(primary_key) == 1 ->
              [{List.first(primary_key), filter}]

            true ->
              filter
          end

        params_with_filter =
          params
          |> Keyword.update(:filter, adjusted_filter, &Kernel.++(&1, adjusted_filter))
          |> Keyword.put(:page, %{limit: 2})

        case read(api, resource, params_with_filter) do
          {:ok, %{results: [single_result]}} ->
            {:ok, single_result}

          {:ok, %{results: []}} ->
            {:ok, nil}

          {:error, error} ->
            {:error, error}

          {:ok, %{results: results}} when is_list(results) ->
            {:error, :too_many_results}
        end

      :error ->
        {:error, "no such resource"}
    end
  end

  def read!(api, resource, params \\ []) do
    api
    |> read(resource, params)
    |> unwrap_or_raise!()
  end

  def read(api, resource, params \\ []) do
    params = add_default_page_size(api, params)

    case api.get_resource(resource) do
      {:ok, resource} ->
        case Keyword.get(params, :action) || Ash.primary_action(resource, :read) do
          nil ->
            {:error, "no action provided, and no primary action found for read"}

          action ->
            Ash.Actions.Read.run(api, resource, action, params)
        end

      :error ->
        {:error, "no such resource"}
    end
  end

  def create!(api, resource, params) do
    api
    |> create(resource, params)
    |> unwrap_or_raise!()
  end

  def create(api, resource, params) do
    case api.get_resource(resource) do
      {:ok, resource} ->
        case Keyword.get(params, :action) || Ash.primary_action(resource, :create) do
          nil ->
            {:error, "no action provided, and no primary action found for create"}

          action ->
            Ash.Actions.Create.run(api, resource, action, params)
        end

      :error ->
        {:error, "no such resource"}
    end
  end

  def update!(api, record, params) do
    api
    |> update(record, params)
    |> unwrap_or_raise!()
  end

  def update(api, %resource{} = record, params) do
    case api.get_resource(resource) do
      {:ok, resource} ->
        case Keyword.get(params, :action) || Ash.primary_action(resource, :update) do
          nil ->
            {:error, "no action provided, and no primary action found for update"}

          action ->
            Ash.Actions.Update.run(api, record, action, params)
        end

      :error ->
        {:error, "no such resource"}
    end
  end

  defp unwrap_or_raise!({:ok, result}), do: result

  defp unwrap_or_raise!({:error, error}) when is_bitstring(error) do
    raise Ash.Error.FrameworkError.exception(message: error)
  end

  defp unwrap_or_raise!({:error, %Ecto.Changeset{}}) do
    raise(Ash.Error.FrameworkError, message: "invalid changes")
  end

  defp unwrap_or_raise!({:error, error}) when not is_list(error) do
    raise error
  end

  defp unwrap_or_raise!({:error, error}) do
    combo_message =
      error
      |> List.wrap()
      |> Stream.map(fn error ->
        case error do
          string when is_bitstring(string) ->
            Ash.Error.FrameworkError.exception(message: string)

          _ = %Ecto.Changeset{} ->
            # TODO: format these
            "invalid changes"

          error ->
            error
        end
      end)
      |> Enum.map_join("\n", &Exception.message/1)

    raise Ash.Error.FrameworkError, message: combo_message
  end

  defp add_default_page_size(api, params) do
    case api.default_page_size() do
      nil ->
        params

      default ->
        with {:ok, page} <- Keyword.fetch(params, :page),
             {:ok, size} when is_integer(size) <- Keyword.fetch(page, :size) do
          params
        else
          _ ->
            Keyword.update(params, :page, [limit: default], &Keyword.put(&1, :limit, default))
        end
    end
  end
end
