defmodule Ash.Test.Resource.CalculationsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Calculation
  alias Ash.Test.Support.PolicySimple.Domain
  alias Ash.Test.Support.PolicySimple.Post

  alias Ash.Test.Domain, as: Domain

  defmacrop defposts(do: body) do
    module = Module.concat(["rand#{System.unique_integer([:positive])}", Post])

    quote do
      defmodule unquote(module) do
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

      alias unquote(module), as: Post
    end
  end

  describe "representation" do
    test "field? option defaults to true and can be overridden" do
      defposts do
        calculations do
          calculate :default_field, :string, concat([:name, :contents])
          calculate :explicit_true_field, :string, concat([:name, :contents]), field?: true
          calculate :false_field, :string, concat([:name, :contents]), field?: false
        end
      end

      calculations = Ash.Resource.Info.calculations(Post)

      default_calc = Enum.find(calculations, &(&1.name == :default_field))
      assert default_calc.field? == true

      explicit_true_calc = Enum.find(calculations, &(&1.name == :explicit_true_field))
      assert explicit_true_calc.field? == true

      false_calc = Enum.find(calculations, &(&1.name == :false_field))
      assert false_calc.field? == false
    end

    test "calculations with field?: false are excluded from struct fields but still accessible via Info" do
      defposts do
        calculations do
          calculate :visible_calc, :string, concat([:name, :contents]), field?: true
          calculate :hidden_calc, :string, concat([:name, :contents]), field?: false
        end

        actions do
          default_accept :*
          defaults [:read, :destroy, update: :*, create: :*]
        end
      end

      # Verify both calculations are accessible via Ash.Resource.Info.calculations/1
      calculations = Ash.Resource.Info.calculations(Post)
      calculation_names = Enum.map(calculations, & &1.name)
      assert :visible_calc in calculation_names
      assert :hidden_calc in calculation_names

      # Check struct fields - field?: true calculations should be present, field?: false should not
      struct_keys = Post.__struct__() |> Map.keys()

      # The visible calculation should appear in struct fields
      assert :visible_calc in struct_keys

      # The hidden calculation should NOT appear in struct fields
      refute :hidden_calc in struct_keys

      # But both should still be accessible via Info functions
      visible_calc = Ash.Resource.Info.calculation(Post, :visible_calc)
      assert visible_calc.name == :visible_calc
      assert visible_calc.field? == true

      hidden_calc = Ash.Resource.Info.calculation(Post, :hidden_calc)
      assert hidden_calc.name == :hidden_calc
      assert hidden_calc.field? == false
    end

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
      defmodule Post1 do
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
          default_accept :*
          defaults [:read, :destroy, update: :*, create: :*]
        end
      end

      defmodule PostName1 do
        @moduledoc """
        Calculates the name of the post, from the child comment.
        """
        use Ash.Resource.Calculation

        @impl true
        def load(_query, _opts, _context), do: [:post]

        @impl true
        def strict_loads?, do: false

        @impl true
        def calculate(records, _opts, _) do
          Enum.map(records, fn comment ->
            post = comment.post
            post.name
          end)
        end
      end

      defmodule Comment1 do
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
          default_accept :*
          defaults [:read, :destroy, update: :*, create: :*]
        end

        relationships do
          belongs_to :post, Post1 do
            public?(true)
          end
        end

        calculations do
          calculate :post_name, :string, PostName1 do
            public?(true)
          end
        end
      end

      post =
        Post1
        |> Ash.Changeset.for_create(:create, %{name: "Post 1", contents: "Contents 1"})
        |> Ash.create!()

      comment =
        Comment1
        |> Ash.Changeset.for_create(:create, %{post_id: post.id})
        |> Ash.create!()

      # assert true == true
      comment_with_post_name = Ash.load!(comment, :post_name)
      assert comment_with_post_name.post_name == post.name
    end

    test "calculations can access attributes of parent in multitenant context" do
      defmodule Post2 do
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
          default_accept :*
          defaults [:read, :destroy, update: :*, create: :*]
        end
      end

      defmodule PostName2 do
        @moduledoc """
        Calculates the name of the post, from the child comment.
        """
        use Ash.Resource.Calculation

        @impl true
        def load(_query, _opts, _context), do: [:post]

        @impl true
        def strict_loads?, do: false

        @impl true
        def calculate(records, _opts, _) do
          Enum.map(records, fn comment ->
            post = comment.post
            post.name
          end)
        end
      end

      defmodule Comment2 do
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
          default_accept :*
          defaults [:read, :destroy, update: :*, create: :*]
        end

        relationships do
          belongs_to :post, Post2 do
            public?(true)
          end
        end

        calculations do
          calculate :post_name, :string, PostName2 do
            public?(true)
          end
        end
      end

      tenant_id = "tenant1"

      post =
        Post2
        |> Ash.Changeset.for_create(:create, %{name: "Post 1", contents: "Contents 1"},
          tenant: tenant_id
        )
        |> Ash.create!()

      comment =
        Comment2
        |> Ash.Changeset.for_create(:create, %{post_id: post.id}, tenant: tenant_id)
        |> Ash.create!()

      comment_with_post_name = Ash.load!(comment, :post_name, tenant: tenant_id)
      assert comment_with_post_name.post_name == post.name
    end
  end

  describe "loading with field?: false" do
    test "field is not added to the struct after loading" do
      defposts do
        calculations do
          calculate :hidden_calc, :string, concat([:name, :contents]), field?: false
        end

        actions do
          default_accept :*
          defaults [:read, :destroy, update: :*, create: :*]
        end
      end

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{name: "Test", contents: "Content"})
        |> Ash.create!()

      # Should raise an error when trying to load a calculation with field?: false
      assert_raise Ash.Error.Invalid, ~r/cannot be loaded directly.*field\?\: false/i, fn ->
        Ash.load!(post, :hidden_calc)
      end
    end
  end
end
