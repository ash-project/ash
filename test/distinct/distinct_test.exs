defmodule Ash.Test.Sort.DistinctTest do
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
      defaults [:read, :create, :update]
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
      end
    end

    calculations do
      calculate :first_title_word, :string, expr(at(string_split(title, " ", trim?: true), 0)) do
        public?(true)
      end

      calculate :second_title_word, :string, expr(at(string_split(title, " ", trim?: true), 1)) do
        public?(true)
      end
    end
  end

  setup do
    Post
    |> Ash.Changeset.for_create(:create, %{title: "fred armisen"})
    |> Domain.create!()

    Post
    |> Ash.Changeset.for_create(:create, %{title: "fred armisen"})
    |> Domain.create!()

    Post
    |> Ash.Changeset.for_create(:create, %{title: "fred weasley"})
    |> Domain.create!()

    :ok
  end

  test "distinct by attribute works" do
    assert [_, _] = Post |> Ash.Query.distinct(:title) |> Domain.read!()
  end

  test "distinct by calculation works" do
    assert [_] =
             Post
             |> Ash.Query.distinct(:first_title_word)
             |> Domain.read!()
  end

  test "distinct_sort calculation works" do
    assert [%{title: "fred armisen"}] =
             Post
             |> Ash.Query.distinct(:first_title_word)
             |> Ash.Query.distinct_sort(:second_title_word)
             |> Domain.read!()

    assert [%{title: "fred weasley"}] =
             Post
             |> Ash.Query.distinct(:first_title_word)
             |> Ash.Query.distinct_sort(second_title_word: :desc)
             |> Domain.read!()
  end
end
