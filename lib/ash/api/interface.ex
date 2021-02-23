defmodule Ash.Api.Interface do
  @moduledoc false

  defmacro __using__(_) do
    quote do
      alias Ash.Api

      def get!(resource, id, params \\ []) do
        Api.get!(__MODULE__, resource, id, params)
      end

      def get(resource, id, params \\ []) do
        case Api.get(__MODULE__, resource, id, params) do
          {:ok, instance} -> {:ok, instance}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def read!(query, opts \\ [])

      def read!(query, opts) do
        Api.read!(__MODULE__, query, opts)
      end

      def read(query, opts \\ [])

      def read(query, opts) do
        case Api.read(__MODULE__, query, opts) do
          {:ok, results, query} -> {:ok, results, query}
          {:ok, results} -> {:ok, results}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def read_one!(query, opts \\ [])

      def read_one!(query, opts) do
        Api.read_one!(__MODULE__, query, opts)
      end

      def read_one(query, opts \\ [])

      def read_one(query, opts) do
        case Api.read_one(__MODULE__, query, opts) do
          {:ok, result} -> {:ok, result}
          {:ok, result, query} -> {:ok, result, query}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def page!(page, request) do
        Api.page!(__MODULE__, page, request)
      end

      def page(page, request) do
        case Api.page(__MODULE__, page, request) do
          {:ok, page} -> {:ok, page}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def load!(data, query, opts \\ []) do
        Api.load!(__MODULE__, data, query, opts)
      end

      def load(data, query, opts \\ []) do
        case Api.load(__MODULE__, data, query, opts) do
          {:ok, results} -> {:ok, results}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def create!(changeset, params \\ []) do
        Api.create!(__MODULE__, changeset, params)
      end

      def create(changeset, params \\ []) do
        case Api.create(__MODULE__, changeset, params) do
          {:ok, instance} -> {:ok, instance}
          {:ok, instance, notifications} -> {:ok, instance, notifications}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def update!(changeset, params \\ []) do
        Api.update!(__MODULE__, changeset, params)
      end

      def update(changeset, params \\ []) do
        case Api.update(__MODULE__, changeset, params) do
          {:ok, instance} -> {:ok, instance}
          {:ok, instance, notifications} -> {:ok, instance, notifications}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def destroy!(record, params \\ []) do
        Api.destroy!(__MODULE__, record, params)
      end

      def destroy(record, params \\ []) do
        case Api.destroy(__MODULE__, record, params) do
          :ok -> :ok
          {:ok, notifications} -> {:ok, notifications}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def reload!(%resource{} = record, params \\ []) do
        id = record |> Map.take(Ash.Resource.Info.primary_key(resource)) |> Enum.to_list()
        get!(resource, id, params)
      end

      def reload(%resource{} = record, params \\ []) do
        id = record |> Map.take(Ash.Resource.Info.primary_key(resource)) |> Enum.to_list()
        get(resource, id, params)
      end
    end
  end
end
