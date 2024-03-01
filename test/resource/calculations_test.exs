defmodule Ash.Test.Resource.CalculationsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Calculation
  alias Ash.Test.Support.PolicySimple.Domain
  alias Ash.Test.Support.PolicySimple.Post

  alias Ash.Test.Domain, as: Domain

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource, domain: Domain

        attributes do
          uuid_primary_key :id

          attribute :name, :string do
            public?(true)
          end

          attribute :contents, :string do
            public?(true)
          end
        end

        unquote(body)
      end
    end
  end

  describe "representation" do
    test "calculations are persisted on the resource properly" do
      defposts do
        calculations do
          calculate :name_and_contents, :string, concat([:name, :context]) do
            public?(true)
          end

          calculate :another_cal_but_private, :string, concat([:name, :context])
        end
      end

      assert [
               %Calculation{
                 name: :name_and_contents,
                 calculation: {Calculation.Concat, [keys: [:name, :context], separator: ""]},
                 public?: true
               },
               %Calculation{
                 name: :another_cal_but_private,
                 calculation: {Calculation.Concat, [keys: [:name, :context], separator: ""]},
                 public?: false
               }
             ] = Ash.Resource.Info.calculations(Post)

      assert [%Calculation{name: :name_and_contents}] =
               Ash.Resource.Info.public_calculations(Post)

      assert %Calculation{name: :another_cal_but_private} =
               Ash.Resource.Info.calculation(Post, :another_cal_but_private)

      assert nil == Ash.Resource.Info.public_calculation(Post, :another_cal_but_private)

      assert nil == Ash.Resource.Info.calculation(Post, :totally_legit_calculation)
    end

    test "Calculation descriptions are allowed" do
      defposts do
        calculations do
          calculate :name_and_contents, :string, concat([:name, :context]),
            public?: true,
            description: "require one of name/contents"
        end
      end

      assert [
               %Ash.Resource.Calculation{description: "require one of name/contents"}
             ] = Ash.Resource.Info.calculations(Post)
    end
  end

  describe "relationships" do
    test "calculations can access attributes of parent" do
      defmodule Post do
        @moduledoc false
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id

          attribute :name, :string do
            public?(true)
          end

          attribute :contents, :string do
            public?(true)
          end
        end

        actions do
          defaults [:read, :update, :destroy, :create]
        end
      end

      defmodule PostName do
        @moduledoc """
        Calculates the name of the post, from the child comment.
        """
        use Ash.Resource.Calculation

        @impl true
        def load(_query, _opts, _context), do: [:post]

        @impl true
        def calculate(records, _opts, _) do
          Enum.map(records, fn comment ->
            post = comment.post
            post.name
          end)
        end
      end

      defmodule Comment do
        @moduledoc false
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        attributes do
          uuid_primary_key :id

          attribute :post_id, :uuid do
            allow_nil?(false)
            public? true
          end
        end

        actions do
          defaults [:read, :update, :destroy, :create]
        end

        relationships do
          belongs_to :post, Post do
            public?(true)
          end
        end

        calculations do
          calculate :post_name, :string, PostName do
            public?(true)
          end
        end
      end

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{name: "Post 1", contents: "Contents 1"})
        |> Domain.create!()

      comment =
        Comment
        |> Ash.Changeset.for_create(:create, %{post_id: post.id})
        |> Domain.create!()

      # assert true == true
      comment_with_post_name = Domain.load!(comment, :post_name)
      assert comment_with_post_name.post_name == post.name
    end

    test "calculations can access attributes of parent in multitenant context" do
      defmodule Post do
        @moduledoc false
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        multitenancy do
          strategy(:context)
        end

        attributes do
          uuid_primary_key :id

          attribute :name, :string do
            public?(true)
          end

          attribute :contents, :string do
            public?(true)
          end
        end

        actions do
          defaults [:read, :update, :destroy, :create]
        end
      end

      defmodule PostName do
        @moduledoc """
        Calculates the name of the post, from the child comment.
        """
        use Ash.Resource.Calculation

        @impl true
        def load(_query, _opts, _context), do: [:post]

        @impl true
        def calculate(records, _opts, _) do
          Enum.map(records, fn comment ->
            post = comment.post
            post.name
          end)
        end
      end

      defmodule Comment do
        @moduledoc false
        use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

        multitenancy do
          strategy(:context)
        end

        attributes do
          uuid_primary_key :id

          attribute :post_id, :uuid do
            public?(true)
            allow_nil?(true)
          end
        end

        actions do
          defaults [:read, :update, :destroy, :create]
        end

        relationships do
          belongs_to :post, Post do
            public?(true)
          end
        end

        calculations do
          calculate :post_name, :string, PostName do
            public?(true)
          end
        end
      end

      tenant_id = "tenant1"

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{name: "Post 1", contents: "Contents 1"},
          tenant: tenant_id
        )
        |> Domain.create!()

      comment =
        Comment
        |> Ash.Changeset.for_create(:create, %{post_id: post.id}, tenant: tenant_id)
        |> Domain.create!()

      comment_with_post_name = Domain.load!(comment, :post_name, tenant: tenant_id)
      assert comment_with_post_name.post_name == post.name
    end
  end
end
