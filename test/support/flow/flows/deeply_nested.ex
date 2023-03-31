defmodule Ash.Test.Flow.Flows.DeeplyNested do
  @moduledoc false
  use Ash.Flow

  steps do
    transaction :foo do
      branch :bar, true do
        custom :baz, fn _, _ -> {:ok, true} end
      end

      map :biz, [1, 2, 3] do
        branch :beef, true do
          custom :belt, fn _, _ -> {:ok, true} end
        end
      end
    end
  end
end
