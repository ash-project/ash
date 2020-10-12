defmodule Ash.Api.Interface do
  @moduledoc false

  defmacro __using__(_) do
    quote do
      alias Ash.Api

      @impl Ash.Api
      def get!(resource, id, params \\ []) do
        Api.get!(__MODULE__, resource, id, params)
      end

      @impl Ash.Api
      def get(resource, id, params \\ []) do
        case Api.get(__MODULE__, resource, id, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      @impl Ash.Api
      def read!(query, opts \\ [])

      def read!(query, opts) do
        Api.read!(__MODULE__, query, opts)
      end

      @impl Ash.Api
      def read(query, opts \\ [])

      def read(query, opts) do
        case Api.read(__MODULE__, query, opts) do
          {:ok, results} -> {:ok, results}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      @impl Ash.Api
      def page!(page, request) do
        Api.page!(__MODULE__, page, request)
      end

      @impl Ash.Api
      def page(page, request) do
        case Api.page(__MODULE__, page, request) do
          {:ok, page} -> {:ok, page}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      @impl Ash.Api
      def load!(data, query, opts \\ []) do
        Api.load!(__MODULE__, data, query, opts)
      end

      @impl Ash.Api
      def load(data, query, opts \\ []) do
        case Api.load(__MODULE__, data, query, opts) do
          {:ok, results} -> {:ok, results}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      @impl Ash.Api
      def create!(changeset, params \\ []) do
        Api.create!(__MODULE__, changeset, params)
      end

      @impl Ash.Api
      def create(changeset, params \\ []) do
        case Api.create(__MODULE__, changeset, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      @impl Ash.Api
      def update!(changeset, params \\ []) do
        Api.update!(__MODULE__, changeset, params)
      end

      @impl Ash.Api
      def update(changeset, params \\ []) do
        case Api.update(__MODULE__, changeset, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      @impl Ash.Api
      def destroy!(record, params \\ []) do
        Api.destroy!(__MODULE__, record, params)
      end

      @impl Ash.Api
      def destroy(record, params \\ []) do
        case Api.destroy(__MODULE__, record, params) do
          :ok -> :ok
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      @impl Ash.Api
      def reload!(%resource{} = record, params \\ []) do
        id = record |> Map.take(Ash.Resource.primary_key(resource)) |> Enum.to_list()
        get!(resource, id, params)
      end

      @impl Ash.Api
      def reload(%resource{} = record, params \\ []) do
        id = record |> Map.take(Ash.Resource.primary_key(resource)) |> Enum.to_list()
        get(resource, id, params)
      end
    end
  end
end
