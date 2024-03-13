defmodule Ash.Test.Sort.SortTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      read :read

      create :create

      update :update
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
      end

      attribute :contents, :string do
        public?(true)
      end

      attribute :points, :integer
    end
  end

  describe "parse_input/2" do
    test "simple string sort parses properly" do
      assert {:ok, [title: :asc, contents: :desc]} =
               Ash.Sort.parse_input(Post, "+title,-contents")
    end

    test "private attributes cannot be used" do
      assert {:error, %Ash.Error.Query.NoSuchField{}} = Ash.Sort.parse_input(Post, "points")
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
