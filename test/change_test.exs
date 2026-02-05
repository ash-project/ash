defmodule Ash.Test.ChangeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule UpdateFirstPublishedAt do
    @moduledoc false
    use Ash.Resource.Change

    @impl true
    def batch_change(changesets, _opts, _context) do
      Enum.map(changesets, fn changeset ->
        if Ash.Changeset.fetch_change(changeset, :published) == {:ok, true} do
          Ash.Changeset.force_change_attribute(changeset, :first_published_at, DateTime.utc_now())
        else
          changeset
        end
      end)
    end
  end

  defmodule SetSlugOnFirstPublish do
    @moduledoc false
    use Ash.Resource.Change

    @impl true
    def change(changeset, _opts, _context) do
      if Ash.Changeset.fetch_change(changeset, :first_published_at) != :error do
        name = Ash.Changeset.get_attribute(changeset, :name) || "untitled"
        slug = name |> String.downcase() |> String.replace(~r/\s+/, "-")
        Ash.Changeset.force_change_attribute(changeset, :slug, slug)
      else
        changeset
      end
    end
  end

  defmodule SomeResource do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Domain

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string
      attribute :slug, :string
      attribute :published, :boolean, default: false
      attribute :first_published_at, :utc_datetime
    end

    actions do
      create :create do
        accept [:name, :published]

        change UpdateFirstPublishedAt
        change SetSlugOnFirstPublish
      end

      update :update do
        accept [:name, :published]
        require_atomic? false

        change UpdateFirstPublishedAt
        change SetSlugOnFirstPublish
      end
    end
  end

  test "changes are applied in the correct order on create even when there is a mix of batch_change and change callbacks" do
    assert {:ok,
            %SomeResource{
              published: true,
              slug: "my-first-post",
              first_published_at: first_published_at
            }} =
             SomeResource
             |> Ash.Changeset.for_create(:create, %{name: "My First Post", published: true})
             |> Ash.create()

    refute is_nil(first_published_at)
  end

  test "changes are applied in the correct order on update even when there is a mix of batch_change and change callbacks" do
    {:ok, resource} =
      SomeResource
      |> Ash.Changeset.for_create(:create, %{name: "My First Post", published: false})
      |> Ash.create()

    assert {:ok,
            %SomeResource{
              published: true,
              slug: "my-first-post",
              first_published_at: first_published_at
            }} =
             resource
             |> Ash.Changeset.for_update(:update, %{published: true})
             |> Ash.update()

    refute is_nil(first_published_at)
  end
end
