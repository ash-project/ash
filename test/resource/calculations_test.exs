defmodule Ash.Test.Resource.CalculationsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmacrop defposts(do: body) do
    quote do
      defmodule Post do
        @moduledoc false
        use Ash.Resource

        attributes do
          attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0

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
          calculate :name_and_contents, concat([:name, :context])
        end
      end

      assert [
               %Ash.Resource.Calculation{
                 name: :name_and_contents,
                 calculation:
                   {Ash.Resource.Calculation.Concat, [keys: [:name, :context], separator: ""]}
               }
             ] = Ash.Resource.calculations(Post)
    end
  end
end
