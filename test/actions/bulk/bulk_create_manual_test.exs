defmodule Ash.Test.Actions.BulkCreateManualTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Expr
  import ExUnit.CaptureLog

  alias Ash.Test.Domain, as: Domain

  defmodule Notifier do
    use Ash.Notifier

    def notify(notification) do
      send(self(), {:notification, notification})
    end
  end

  defmodule CreateManual do
    use Ash.Resource.ManualCreate

    def create(changeset, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      changeset.resource
      |> Ash.Changeset.for_create(:create, Map.take(changeset.attributes, [:name]), opts)
      |> Ash.create(opts)
    end

    def bulk_create(changesets, _module_opts, ctx) do
      opts = Ash.Context.to_opts(ctx)

      Enum.reduce(changesets, [], fn changeset, results ->
        result =
          changeset.resource
          |> Ash.Changeset.for_create(:create, Map.take(changeset.attributes, [:name]), opts)
          |> Ash.create(opts ++ [return_notifications?: true])
          |> case do
            {:ok, record, notification} ->
              record =
                Ash.Resource.put_metadata(
                  record,
                  :bulk_create_index,
                  changeset.context.bulk_create.index
                )

              [{:ok, record, notification} | results]

            {:error, error} ->
              [{:error, error} | results]
          end
      end)
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :update, :destroy]

      create :create_manual do
        accept [:name]
        manual CreateManual
      end

      create :create do
        accept [:name]
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public? true
        allow_nil? false
      end
    end
  end

  test "bulk_create works on manual action with bulk_create/3" do
    result =
      [%{name: "Author1"}, %{name: "Author2"}, %{name: "Author3"}]
      |> Ash.bulk_create(Author, :create_manual,
        return_notifications?: true,
        return_errors?: true,
        return_records?: true
      )

    assert Enum.count(result.records) == 3
    assert Enum.count(result.notifications) == 3
    assert result.error_count == 0
  end

  test "bulk_create on manual action with bulk_create/3 fails with invalid inputs" do
    result =
      [%{name: "Author1"}, %{}, %{name: "Author3"}]
      |> Ash.bulk_create(Author, :create_manual,
        return_notifications?: true,
        return_errors?: true,
        return_records?: true
      )

    assert Enum.empty?(result.records)
    assert result.notifications == nil
    assert result.error_count == 1
  end
end
