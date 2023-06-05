defmodule Ash.Test.Flow.Flows.Halting do
  @moduledoc false
  use Ash.Flow

  flow do
    argument :on_step, :atom do
      constraints(one_of: [:a, :b, :c])
    end

    returns(:c)
  end

  steps do
    custom :a, Ash.Test.Flow.Steps.SimpleReturn do
      input(%{return: "a"})
      halt_if(expr(not (^arg(:on_step) == :a || false)))
      halt_reason(:not_on_step_a)
    end

    custom :b, Ash.Test.Flow.Steps.SimpleReturn do
      input(%{return: "b"})
      halt_if(expr(not (^arg(:on_step) == :b || false)))
      wait_for(result(:a))
      halt_reason(:not_on_step_b)
    end

    custom :c, Ash.Test.Flow.Steps.SimpleReturn do
      input(%{return: "c"})
      halt_if(expr(not (^arg(:on_step) == :c || false)))
      wait_for(result(:b))
      halt_reason(:not_on_step_c)
    end
  end
end
