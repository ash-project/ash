defmodule Ash.Api.Interface do
  defmacro __using__(_) do
    quote do
      @spec get!(Ash.resource(), term(), Ash.params()) :: Ash.record() | no_return
      def get!(resource, id, params \\ []) do
        Ash.Api.Interface.get!(__MODULE__, resource, id, params)
      end

      @spec get(Ash.resource(), term(), Ash.params()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}
      def get(resource, id, params \\ []) do
        case Ash.Api.Interface.get(__MODULE__, resource, id, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      @spec read!(Ash.resource(), Ash.params()) :: Ash.page() | no_return
      def read!(resource, params \\ []) do
        Ash.Api.Interface.read!(__MODULE__, resource, params)
      end

      @spec read(Ash.resource(), Ash.params()) :: {:ok, Ash.page()} | {:error, Ash.error()}
      def read(resource, params \\ []) do
        case Ash.Api.Interface.read(__MODULE__, resource, params) do
          {:ok, paginator} -> {:ok, paginator}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      @spec create!(Ash.resource(), Ash.create_params()) :: Ash.record() | no_return
      def create!(resource, params \\ []) do
        Ash.Api.Interface.create!(__MODULE__, resource, params)
      end

      @spec create(Ash.resource(), Ash.create_params()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}
      def create(resource, params \\ []) do
        case Ash.Api.Interface.create(__MODULE__, resource, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      @spec update!(Ash.record(), Ash.update_params()) :: Ash.record() | no_return
      def update!(record, params \\ []) do
        Ash.Api.Interface.update!(__MODULE__, record, params)
      end

      @spec update(Ash.record(), Ash.update_params()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}
      def update(record, params \\ []) do
        case Ash.Api.Interface.update(__MODULE__, record, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      @spec destroy!(Ash.record(), Ash.update_params()) :: Ash.record() | no_return
      def destroy!(record, params \\ []) do
        Ash.Api.Interface.destroy!(__MODULE__, record, params)
      end

      @spec destroy(Ash.record(), Ash.update_params()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}
      def destory(record, params \\ []) do
        case Ash.Api.Interface.destroy(__MODULE__, record, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end
    end
  end

  @spec get!(Ash.api(), Ash.resource(), term(), Ash.params()) :: Ash.record() | no_return
  def get!(api, resource, id, params \\ []) do
    api
    |> get(resource, id, params)
    |> unwrap_or_raise!()
  end

  @spec get(Ash.api(), Ash.resource(), term(), Ash.params()) ::
          {:ok, Ash.record()} | {:error, Ash.error()}
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
        {:error, "no such resource #{resource}"}
    end
  end

  @spec read!(Ash.api(), Ash.resource(), Ash.params()) :: Ash.page() | no_return
  def(read!(api, resource, params \\ [])) do
    api
    |> read(resource, params)
    |> unwrap_or_raise!()
  end

  @spec read(Ash.api(), Ash.resource(), Ash.params()) ::
          {:ok, Ash.page()} | {:error, Ash.error()}
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
        {:error, "no such resource #{resource}"}
    end
  end

  @spec create!(Ash.api(), Ash.resource(), Ash.create_params()) ::
          Ash.record() | {:error, Ash.error()}
  def create!(api, resource, params) do
    api
    |> create(resource, params)
    |> unwrap_or_raise!()
  end

  @spec create(Ash.api(), Ash.resource(), Ash.create_params()) ::
          {:ok, Ash.resource()} | {:error, Ash.error()}
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
        {:error, "no such resource #{resource}"}
    end
  end

  @spec update!(Ash.api(), Ash.record(), Ash.update_params()) :: Ash.resource() | no_return
  def update!(api, record, params) do
    api
    |> update(record, params)
    |> unwrap_or_raise!()
  end

  @spec update(Ash.api(), Ash.record(), Ash.update_params()) ::
          {:ok, Ash.resource()} | {:error, Ash.error()}
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
        {:error, "no such resource #{resource}"}
    end
  end

  @spec destroy!(Ash.api(), Ash.record(), Ash.delete_params()) :: Ash.resource() | no_return
  def destroy!(api, record, params) do
    api
    |> destroy(record, params)
    |> unwrap_or_raise!()
  end

  @spec destroy(Ash.api(), Ash.record(), Ash.delete_params()) ::
          {:ok, Ash.resource()} | {:error, Ash.error()}
  def destroy(api, %resource{} = record, params) do
    case api.get_resource(resource) do
      {:ok, resource} ->
        case Keyword.get(params, :action) || Ash.primary_action(resource, :destroy) do
          nil ->
            {:error, "no action provided, and no primary action found for destroy"}

          action ->
            Ash.Actions.Destroy.run(api, record, action, params)
        end

      :error ->
        {:error, "no such resource #{resource}"}
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
