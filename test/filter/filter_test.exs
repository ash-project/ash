defmodule Ash.Test.Filter.FilterTest do
  use ExUnit.Case, async: true

  defmodule User do
    use Ash.Resource, name: "users", type: "user"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default
      create :default
    end

    attributes do
      attribute :name, :string
      attribute :allow_second_author, :boolean
    end

    relationships do
      has_many :posts, Ash.Test.Filter.FilterTest.Post
    end
  end

  defmodule Post do
    use Ash.Resource, name: "posts", type: "post"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default

      create :default
    end

    attributes do
      attribute :title, :string
      attribute :contents, :string
    end

    relationships do
      belongs_to :author1, User,
        destination_field: :id,
        source_field: :author1_id

      belongs_to :author2, User,
        destination_field: :id,
        source_field: :author2_id
    end
  end

  defmodule Api do
    use Ash.Api

    resources [Post, Author]
  end

  test "it works" do
    authorization_steps = [
      authorize_if: [author1: [id: 1]],
      forbid_if: [author1: [allow_second_author: false]],
      authorize_if: [author2: [id: 1]]
    ]

    Ash.Filter.SatSolver.solve(Post, authorization_steps)

    # Api.read(Post,
    #   filter: [
    #     title: "foo",
    #     title: "bar",
    #     contents: [in: ["bar", "baz"]],
    #     contents: [in: ["foo", "bar"]],
    #     author1: [id: 1],
    #     author2: [name: "bob"]
    #   ]
    # )
  end
end
