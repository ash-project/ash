defmodule Ash.Api.Interface do
  defmacro __using__(_) do
    quote do
      def get(resource, id, params \\ %{}) do
        Ash.Api.Interface.get(__MODULE__, resource, id, params)
      end

      def read(resource, params) do
        Ash.Api.Interface.read(__MODULE__, resource, params)
      end

      def create(resource, params) do
        Ash.Api.Interface.create(__MODULE__, resource, params)
      end
    end
  end

  def get(api, resource, id, params \\ %{}) do
    # TODO: Figure out this interface
    params_with_filter =
      params
      |> Map.put_new(:filter, %{})
      |> Map.update!(:filter, &Map.put(&1, :id, id))
      |> Map.put(:page, %{limit: 2})

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
  end

  def read(api, resource, params \\ %{}) do
    params = add_default_page_size(api, params)

    case Map.get(params, :action) || Ash.primary_action(resource, :read) do
      nil ->
        {:error, "no action provided, and no primary action found"}

      action ->
        Ash.DataLayer.Actions.run_read_action(resource, action, api, params)
    end
  end

  def create(api, resource, params) do
    case Map.get(params, :action) || Ash.primary_action(resource, :create) do
      nil ->
        {:error, "no action provided, and no primary action found"}

      action ->
        Ash.DataLayer.Actions.run_create_action(resource, action, api, params)
    end
  end

  defp add_default_page_size(_api, %{page: %{limit: value}} = params) when is_integer(value) do
    params
  end

  defp add_default_page_size(api, params) do
    params
    |> Map.update(
      :page,
      %{limit: api.default_page_size},
      &Map.put(&1, :limit, api.default_page_size)
    )
  end
end
