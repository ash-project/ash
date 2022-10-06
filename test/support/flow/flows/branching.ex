defmodule Ash.Test.Flow.Flows.Branching do
  @moduledoc false
  use Ash.Flow

  flow do
    argument :do_branch, :boolean do
      allow_nil? false
      default false
    end

    argument :do_inner_branch, :boolean do
      allow_nil? false
      default false
    end

    argument :do_second_branch, :boolean do
      allow_nil? false
      default false
    end

    returns [:branch, :second_branch]
  end

  steps do
    branch :branch, arg(:do_branch) do
      branch :inner_branch, arg(:do_inner_branch) do
        custom :inner_branch_return, Ash.Test.Flow.Steps.SimpleReturn do
          input %{return: "inner_branch happened"}
        end
      end

      branch :inner_branch_alt, expr(not (^arg(:do_inner_branch))) do
        custom :inner_branch_alt_return, Ash.Test.Flow.Steps.SimpleReturn do
          input %{return: "inner_branch didn't happen"}
        end
      end

      custom :branch_result, Ash.Test.Flow.Steps.SimpleReturn do
        input %{return: expr(^result(:inner_branch) || ^result(:inner_branch_alt))}
      end
    end

    branch :second_branch, arg(:do_second_branch) do
      custom :second_branch_return, Ash.Test.Flow.Steps.SimpleReturn do
        input %{return: "second_branch happened"}
      end
    end
  end
end
