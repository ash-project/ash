defmodule Ash.Authorization.Checker do
  def strict_check(user, request, facts) do
    Enum.reduce(
      request.authorization_steps,
      {facts, []},
      fn {
           _step_type,
           condition
         },
         {facts, instructions} ->
        case Map.fetch(facts, {request.relationship, condition}) do
          {:ok, _boolean_result} ->
            {facts, instructions}

          :error ->
            case do_strict_check(condition, user, request) do
              {:unknown, new_instructions} ->
                {facts, instructions ++ new_instructions}

              {boolean, new_instructions} ->
                {Map.put(facts, {request.relationship, condition}, boolean),
                 instructions ++ new_instructions}
            end
        end
      end
    )
  end

  defp do_strict_check({module, opts}, user, request) do
    strict_check_results = module.strict_check(user, request, opts)

    case Keyword.fetch(strict_check_results, :decision) do
      {:ok, boolean} -> {boolean, []}
      :error -> {:unknown, []}
    end
  end
end
