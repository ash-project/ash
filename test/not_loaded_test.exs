# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.NotLoadedTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

    resources do
      allow_unregistered? true
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
      attribute :body, :string, public?: true
    end

    relationships do
      belongs_to :author, __MODULE__, public?: true
    end

    calculations do
      calculate :title_upcase, :string, expr(string_upcase(title)) do
        public? true
      end
    end

    aggregates do
      count :count_of_related, :author, public?: true
    end
  end

  describe "struct" do
    test "resource field defaults to nil" do
      not_loaded = %Ash.NotLoaded{type: :relationship, field: :author}
      assert not_loaded.resource == nil
    end

    test "resource field can be set" do
      not_loaded = %Ash.NotLoaded{type: :relationship, field: :author, resource: Post}
      assert not_loaded.resource == Post
    end
  end

  describe "resource in struct defaults" do
    test "relationship fields have resource set in struct default" do
      post = struct(Post)
      assert %Ash.NotLoaded{resource: Post, type: :relationship} = post.author
    end

    test "aggregate fields have resource set in struct default" do
      post = struct(Post)
      assert %Ash.NotLoaded{resource: Post, type: :aggregate} = post.count_of_related
    end

    test "calculation fields have resource set in struct default" do
      post = struct(Post)
      assert %Ash.NotLoaded{resource: Post, type: :calculation} = post.title_upcase
    end
  end

  describe "inspect" do
    test "shows resource when inspected standalone" do
      not_loaded = %Ash.NotLoaded{type: :relationship, field: :author, resource: Post}
      result = inspect(not_loaded)
      assert result =~ "resource:"
      assert result =~ "Post"
    end

    test "omits resource when resource is nil" do
      not_loaded = %Ash.NotLoaded{type: :relationship, field: :author}
      result = inspect(not_loaded)
      refute result =~ "resource:"
    end

    test "hides resource inside a record inspect" do
      post = struct(Post)
      result = inspect(post)
      assert result =~ "NotLoaded"
      refute result =~ "resource:"
    end

    test "hides resource when in_resource? custom option is set" do
      not_loaded = %Ash.NotLoaded{type: :relationship, field: :author, resource: Post}
      opts = %Inspect.Opts{custom_options: [in_resource?: true]}

      result =
        Inspect.Algebra.format(Inspect.inspect(not_loaded, opts), 80)
        |> IO.iodata_to_binary()

      refute result =~ "resource:"
    end

    test "shows resource when in_resource? is not set" do
      not_loaded = %Ash.NotLoaded{type: :relationship, field: :author, resource: Post}
      opts = %Inspect.Opts{}

      result =
        Inspect.Algebra.format(Inspect.inspect(not_loaded, opts), 80)
        |> IO.iodata_to_binary()

      assert result =~ "resource:"
      assert result =~ "Post"
    end
  end
end
