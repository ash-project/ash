defmodule Ash.Test.Type.AutoTypeErrorsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  describe "auto type with non-expression calculation" do
    test "raises a clear error" do
      assert_raise Spark.Error.DslError,
                   ~r/`:auto` type is only supported for expression calculations/,
                   fn ->
                     defmodule BadCalcResource do
                       use Ash.Resource,
                         domain: Ash.Test.Domain,
                         data_layer: Ash.DataLayer.Ets

                       ets do
                         private?(true)
                       end

                       actions do
                         default_accept :*
                         defaults [:read, create: :*]
                       end

                       attributes do
                         uuid_primary_key :id
                         attribute :title, :string, public?: true
                       end

                       calculations do
                         calculate :bad_calc, :auto, fn records, _ ->
                           Enum.map(records, fn _ -> "nope" end)
                         end
                       end
                     end
                   end
    end
  end
end
