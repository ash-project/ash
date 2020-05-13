defmodule Ash.Api.Interface do
  @moduledoc """
  The primary entry point for interacting with resources and their data.

  #TODO describe - Big picture description here
  """

  alias Ash.Error.Interface.NoSuchResource

  @authorization_schema Ashton.schema(
                          opts: [
                            user: :any,
                            strict_access?: :boolean
                          ],
                          defaults: [strict_access?: true],
                          describe: [
                            user: "# TODO describe",
                            strict_access?:
                              "only applies to `read` actions, so maybe belongs somewhere else"
                          ]
                        )

  @global_opts Ashton.schema(
                 opts: [
                   authorization: [{:const, false}, @authorization_schema],
                   verbose?: :boolean
                 ],
                 defaults: [
                   authorization: false,
                   verbose?: false
                 ],
                 describe: [
                   authorization: "# TODO describe",
                   verbose?: "Debug log engine operation"
                 ]
               )

  @read_opts_schema []
                    |> Ashton.schema()
                    |> Ashton.merge(@global_opts, annotate: "Global Opts")

  @get_opts_schema []
                   |> Ashton.schema()
                   |> Ashton.merge(@global_opts, annotate: "Global Opts")

  @create_and_update_opts_schema [
                                   opts: [
                                     attributes: :map,
                                     relationships: :map
                                   ],
                                   defaults: [
                                     attributes: %{},
                                     relationships: %{}
                                   ],
                                   describe: [
                                     attributes: "#TODO describe",
                                     relationships: "#TODO describe"
                                   ]
                                 ]
                                 |> Ashton.schema()
                                 |> Ashton.merge(@global_opts, annotate: "Global Opts")

  @delete_opts_schema []
                      |> Ashton.schema()
                      |> Ashton.merge(@global_opts, annotate: "Global Opts")

  @doc """
  #TODO describe

  #{Ashton.document(@get_opts_schema)}
  """
  @callback get!(resource :: Ash.resource(), id_or_filter :: term(), params :: Ash.params()) ::
              Ash.record() | no_return

  @doc """
  #TODO describe

  #{Ashton.document(@get_opts_schema)}
  """
  @callback get(resource :: Ash.resource(), id_or_filter :: term(), params :: Ash.params()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}

  @doc """
  #TODO describe

  #{Ashton.document(@read_opts_schema)}
  """
  @callback read!(resource :: Ash.resource(), params :: Ash.params()) ::
              list(Ash.resource()) | no_return

  @doc """
  #TODO describe

  #{Ashton.document(@read_opts_schema)}
  """
  @callback read(resource :: Ash.resource(), params :: Ash.params()) ::
              {:ok, list(Ash.resource())} | {:error, Ash.error()}

  @doc """
  #TODO describe

  #{Ashton.document(@create_and_update_opts_schema)}
  """
  @callback create!(resource :: Ash.resource(), params :: Ash.create_params()) ::
              Ash.record() | no_return

  @doc """
  #TODO describe

  #{Ashton.document(@create_and_update_opts_schema)}
  """
  @callback create(resource :: Ash.resource(), params :: Ash.create_params()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}

  @doc """
  #TODO describe

  #{Ashton.document(@create_and_update_opts_schema)}
  """
  @callback update!(record :: Ash.record(), params :: Ash.update_params()) ::
              Ash.record() | no_return

  @doc """
  #TODO describe

  #{Ashton.document(@create_and_update_opts_schema)}
  """
  @callback update(record :: Ash.record(), params :: Ash.update_params()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}

  @doc """
  #TODO describe

  #{Ashton.document(@delete_opts_schema)}
  """
  @callback destroy!(record :: Ash.record(), params :: Ash.update_params()) ::
              Ash.record() | no_return

  @doc """
  #TODO describe

  #{Ashton.document(@delete_opts_schema)}
  """
  @callback destroy(record :: Ash.record(), params :: Ash.update_params()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}

  @doc """
  Refetches a record from the database
  """
  @callback reload(record :: Ash.record(), params :: Ash.params()) ::
              {:ok, Ash.record()} | {:error, Ash.error()}

  @doc """
  Refetches a record from the database, raising on error.

  See `reload/1`.
  """
  @callback reload!(record :: Ash.record(), params :: Ash.params()) :: Ash.record() | no_return

  @doc """
  Refetches a record from the database
  """
  @callback reload(record :: Ash.record()) :: {:ok, Ash.record()} | {:error, Ash.error()}

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Api.Interface

      @impl true
      def get!(resource, id, params \\ []) do
        Ash.Api.Interface.get!(__MODULE__, resource, id, params)
      end

      @impl true
      def get(resource, id, params \\ []) do
        case Ash.Api.Interface.get(__MODULE__, resource, id, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      @impl true
      def read!(query, opts \\ [])

      def read!(resource, opts) when is_atom(resource) do
        read!(query(resource), opts)
      end

      def read!(query, opts) do
        Ash.Api.Interface.read!(__MODULE__, query, opts)
      end

      @impl true
      def read(query, opts \\ [])

      def read(resource, opts) when is_atom(resource) do
        read(query(resource), opts)
      end

      def read(query, opts) do
        case Ash.Api.Interface.read(__MODULE__, query, opts) do
          {:ok, results} -> {:ok, results}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      @impl true
      def create!(resource, params \\ []) do
        Ash.Api.Interface.create!(__MODULE__, resource, params)
      end

      @impl true
      def create(resource, params \\ []) do
        case Ash.Api.Interface.create(__MODULE__, resource, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      @impl true
      def update!(record, params \\ []) do
        Ash.Api.Interface.update!(__MODULE__, record, params)
      end

      @impl true
      def update(record, params \\ []) do
        case Ash.Api.Interface.update(__MODULE__, record, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      @impl true
      def destroy!(record, params \\ []) do
        Ash.Api.Interface.destroy!(__MODULE__, record, params)
      end

      @impl true
      def destroy(record, params \\ []) do
        case Ash.Api.Interface.destroy(__MODULE__, record, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, List.wrap(error)}
        end
      end

      @impl true
      def reload!(%resource{} = record, params \\ []) do
        id = record |> Map.take(Ash.primary_key(resource)) |> Enum.to_list()
        get!(resource, id, params)
      end

      @impl true
      def reload(%resource{} = record, params \\ []) do
        id = record |> Map.take(Ash.primary_key(resource)) |> Enum.to_list()
        get(resource, id, params)
      end

      def query(resource) do
        Ash.Query.new(__MODULE__, resource)
      end
    end
  end

  @doc false
  @spec get!(Ash.api(), Ash.resource(), term(), Ash.params()) :: Ash.record() | no_return
  def get!(api, resource, id, params \\ []) do
    api
    |> get(resource, id, params)
    |> unwrap_or_raise!()
  end

  @doc false
  @spec get(Ash.api(), Ash.resource(), term(), Keyword.t()) ::
          {:ok, Ash.record()} | {:error, Ash.error()}
  def get(api, resource, id, opts) do
    with {:resource, {:ok, resource}} <- {:resource, api.get_resource(resource)},
         {:pkey, primary_key} when primary_key != [] <- {:pkey, Ash.primary_key(resource)} do
      filter =
        case {primary_key, id} do
          {[field], [{field, value}]} ->
            {:ok, [{field, value}]}

          {[field], value} ->
            {:ok, [{field, value}]}

          {fields, value} ->
            if Keyword.keyword?(value) and Enum.sort(Keyword.keys(value)) == Enum.sort(fields) do
              {:ok, value}
            else
              {:error, "invalid primary key provided to `get/3`"}
            end
        end

      case filter do
        {:ok, filter} ->
          resource
          |> api.query()
          |> Ash.Query.filter(filter)
          |> Ash.Query.side_load(opts[:side_load] || [])
          |> api.read(opts)
          |> case do
            {:ok, [single_result]} ->
              {:ok, single_result}

            {:ok, []} ->
              {:ok, nil}

            {:error, error} ->
              {:error, error}

            {:ok, results} when is_list(results) ->
              {:error, :too_many_results}
          end

        {:error, error} ->
          {:error, error}
      end
    else
      {:resource, :error} ->
        {:error, NoSuchResource.exception(resource: resource)}
    end
  end

  @doc false
  @spec read!(Ash.api(), Ash.query(), Keyword.t()) ::
          list(Ash.resource()) | no_return
  def read!(api, query, opts \\ []) do
    api
    |> read(query, opts)
    |> unwrap_or_raise!()
  end

  @doc false
  @spec read(Ash.api(), Ash.query(), Keyword.t()) ::
          {:ok, list(Ash.resource())} | {:error, Ash.error()}
  def read(_api, query, opts \\ []) do
    Ash.Actions.Read.run(query, opts)
  end

  @doc false
  @spec create!(Ash.api(), Ash.resource(), Ash.create_params()) ::
          Ash.record() | {:error, Ash.error()}
  def create!(api, resource, params) do
    api
    |> create(resource, params)
    |> unwrap_or_raise!()
  end

  @doc false
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
        {:error, NoSuchResource.exception(resource: resource)}
    end
  end

  @doc false
  @spec update!(Ash.api(), Ash.record(), Ash.update_params()) :: Ash.resource() | no_return
  def update!(api, record, params) do
    api
    |> update(record, params)
    |> unwrap_or_raise!()
  end

  @doc false
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
        {:error, NoSuchResource.exception(resource: resource)}
    end
  end

  @doc false
  @spec destroy!(Ash.api(), Ash.record(), Ash.delete_params()) :: Ash.resource() | no_return
  def destroy!(api, record, params) do
    api
    |> destroy(record, params)
    |> unwrap_or_raise!()
  end

  @doc false
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
        {:error, NoSuchResource.exception(resource: resource)}
    end
  end

  defp unwrap_or_raise!(:ok), do: :ok
  defp unwrap_or_raise!({:ok, result}), do: result
  defp unwrap_or_raise!({:error, error}), do: raise(Ash.to_ash_error(error))
end
