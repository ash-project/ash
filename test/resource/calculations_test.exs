defmodule Ash.Test.Resource.CalculationsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Resource.Calculation

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource

        attributes do
          uuid_primary_key :id

          attribute :name, :string
          attribute :contents, :string
        end

        unquote(body)
      end
    end
  end

  describe "representation" do
    test "calculations are persisted on the resource properly" do
      defposts do
        calculations do
          calculate :name_and_contents, :string, concat([:name, :context])
          calculate(:another_cal_but_private, :string, concat([:name, :context]), private?: true)
        end
      end

      assert [
               %Calculation{
                 name: :name_and_contents,
                 calculation: {Calculation.Concat, [keys: [:name, :context], separator: ""]},
                 private?: false
               },
               %Calculation{
                 name: :another_cal_but_private,
                 calculation: {Calculation.Concat, [keys: [:name, :context], separator: ""]},
                 private?: true
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
          calculate(:name_and_contents, :string, concat([:name, :context]),
            description: "require one of name/contents"
          )
        end
      end

      assert [
               %Ash.Resource.Calculation{description: "require one of name/contents"}
             ] = Ash.Resource.Info.calculations(Post)
    end
  end
end
