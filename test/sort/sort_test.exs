defmodule Ash.Test.Sort.SortTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :default

      create :default

      update :default
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string
      attribute :contents, :string
      attribute :points, :integer, private?: true
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource(Post)
    end
  end

  describe "parse_input/2" do
    test "simple string sort parses properly" do
      assert {:ok, [title: :asc, contents: :desc]} =
               Ash.Sort.parse_input(Post, "+title,-contents")
    end

    test "public attributes cannot be used" do
      assert {:error, %Ash.Error.Query.NoSuchAttribute{}} = Ash.Sort.parse_input(Post, "points")
    end

    test "a list sort parses properly" do
      assert {:ok, [title: :asc, contents: :desc]} =
               Ash.Sort.parse_input(Post, ["title", "-contents"])
    end

    test "a regular sort parses properly" do
      assert {:ok, [title: :asc, contents: :desc]} =
               Ash.Sort.parse_input(Post, title: :asc, contents: :desc)
    end

    test "++ and -- modifiers work properly" do
      assert {:ok, [title: :asc_nils_first, contents: :desc_nils_last]} =
               Ash.Sort.parse_input(Post, "++title,--contents")
    end
  end
end
