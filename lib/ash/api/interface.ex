defmodule Ash.Api.Interface do
  @moduledoc """
  The primary entry point for interacting with resources and their data.

  #TODO describe - Big picture description here
  """

  @authorization_schema Ashton.schema(
                          opts: [
                            user: :any,
                            strict_access?: :boolean,
                            log_final_report?: :boolean
                          ],
                          defaults: [strict_access?: true, log_final_report?: false],
                          describe: [
                            user: "# TODO describe",
                            log_final_report?:
                              "If true, an info log with an auth report is emitted on successful authorization",
                            strict_access?:
                              "only applies to `read` actions, so maybe belongs somewhere else"
                          ]
                        )

  @global_opts Ashton.schema(
                 opts: [
                   authorization: [{:const, false}, @authorization_schema]
                 ],
                 defaults: [
                   authorization: false
                 ],
                 describe: [
                   authorization: "# TODO describe"
                 ]
               )

  @shared_read_get_opts_schema Ashton.schema(
                                 opts: [
                                   side_load: :keyword,
                                   side_load_filter: :map
                                 ],
                                 defaults: [
                                   side_load: [],
                                   side_load_filter: %{}
                                 ],
                                 describe: [
                                   side_load: "# TODO describe",
                                   side_load_filter: "# TODO describe"
                                 ]
                               )

  @pagination_schema Ashton.schema(
                       opts: [
                         limit: :integer,
                         offset: :integer
                       ],
                       constraints: [
                         limit: {&Ash.Constraints.positive?/1, "must be positive"},
                         offset: {&Ash.Constraints.positive?/1, "must be positive"}
                       ]
                     )

  @read_opts_schema [
                      opts: [
                        filter: :keyword,
                        sort: {:list, {:tuple, {[{:enum, [:asc, :desc]}], :atom}}},
                        page: [@pagination_schema]
                      ],
                      defaults: [
                        filter: [],
                        sort: [],
                        page: []
                      ],
                      describe: [
                        filter: "# TODO describe",
                        sort: "# TODO describe",
                        page: "# TODO describe"
                      ]
                    ]
                    |> Ashton.schema()
                    |> Ashton.merge(@shared_read_get_opts_schema, annotate: "Shared Read Opts")
                    |> Ashton.merge(@global_opts, annotate: "Global Opts")

  @get_opts_schema []
                   |> Ashton.schema()
                   |> Ashton.merge(@shared_read_get_opts_schema, annotate: "Shared Read Opts")
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
  @callback read!(resource :: Ash.resource(), params :: Ash.params()) :: Ash.page() | no_return

  @doc """
  #TODO describe

  #{Ashton.document(@read_opts_schema)}
  """
  @callback read(resource :: Ash.resource(), params :: Ash.params()) ::
              {:ok, Ash.page()} | {:error, Ash.error()}

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
      def read!(resource, params \\ []) do
        Ash.Api.Interface.read!(__MODULE__, resource, params)
      end

      @impl true
      def read(resource, params \\ []) do
        case Ash.Api.Interface.read(__MODULE__, resource, params) do
          {:ok, paginator} -> {:ok, paginator}
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
  @spec get(Ash.api(), Ash.resource(), term(), Ash.params()) ::
          {:ok, Ash.record()} | {:error, Ash.error()}
  def get(api, resource, filter, params) do
    with {:resource, {:ok, resource}} <- {:resource, api.get_resource(resource)},
         {:pkey, primary_key} when primary_key != [] <- {:pkey, Ash.primary_key(resource)} do
      params =
        Keyword.update(params, :authorization, false, fn authorization ->
          if authorization do
            Keyword.put(authorization, :strict_access?, false)
          else
            authorization
          end
        end)

      adjusted_filter =
        case {primary_key, filter} do
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

      case adjusted_filter do
        {:ok, adjusted_filter} ->
          params_with_filter =
            params
            |> Keyword.update(:filter, adjusted_filter, &Kernel.++(&1, adjusted_filter))
            |> Keyword.put(:page, %{limit: 2})
            |> Keyword.put(:enforce_filter_access, false)

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

        {:error, error} ->
          {:error, error}
      end
    else
      {:resource, :error} ->
        {:error, "no such resource #{resource}"}
    end
  end

  @doc false
  @spec read!(Ash.api(), Ash.resource(), Ash.params()) :: Ash.page() | no_return
  def(read!(api, resource, params \\ [])) do
    api
    |> read(resource, params)
    |> unwrap_or_raise!()
  end

  @doc false
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
        {:error, "no such resource #{resource}"}
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
        {:error, "no such resource #{resource}"}
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
        {:error, "no such resource #{resource}"}
    end
  end

  defp unwrap_or_raise!({:ok, result}), do: result

  defp unwrap_or_raise!({:error, error}) when is_bitstring(error) do
    raise Ash.Error.FrameworkError.exception(message: error)
  end

  defp unwrap_or_raise!({:error, %Ecto.Changeset{} = changeset}) do
    raise(Ash.Error.FrameworkError, message: "invalid changes #{inspect(changeset)}")
  end

  defp unwrap_or_raise!({:error, error}) when is_map(error) do
    # TODO: handle these better
    case deep_find_forbidden(error) do
      nil ->
        raise Ash.Error.FrameworkError, message: "Engine errors: #{inspect(error)}"

      forbidden ->
        raise forbidden
    end
  end

  defp unwrap_or_raise!({:error, error}) when not is_list(error) do
    raise error
  end

  defp unwrap_or_raise!({:error, error}) do
    combo_message =
      error
      |> List.wrap()
      |> Enum.map(fn error ->
        case error do
          string when is_bitstring(string) ->
            Ash.Error.FrameworkError.exception(message: string)

          _ = %Ecto.Changeset{} = changeset ->
            # TODO: format these
            "invalid changes #{inspect(changeset)}"

          error ->
            error
        end
      end)
      |> Enum.reject(fn error ->
        # A lot of the error logic here, including this annoying scrubbing code
        # is temporary.
        error == []
      end)
      |> Enum.map_join("\n", &Exception.message/1)

    raise Ash.Error.FrameworkError, message: combo_message
  end

  defp deep_find_forbidden(errors, path \\ [])

  defp deep_find_forbidden(errors, path) when is_list(errors) do
    errors
    |> Enum.with_index()
    |> Enum.find_value(fn {error, index} ->
      case deep_find_forbidden(error, [index | path]) do
        nil ->
          nil

        error ->
          error
      end
    end)
  end

  defp deep_find_forbidden(%Ash.Error.Forbidden{} = forbidden, path) do
    Map.put(forbidden, :path, Enum.reverse(path))
  end

  defp deep_find_forbidden(%_{}, _), do: nil

  defp deep_find_forbidden(errors, path) when is_map(errors) do
    Enum.find_value(errors, fn {key, value} ->
      case deep_find_forbidden(value, [key | path]) do
        nil ->
          nil

        error ->
          error
      end
    end)
  end

  defp deep_find_forbidden(_, _) do
    nil
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
