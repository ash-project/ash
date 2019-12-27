defmodule Ash.Authorization.Checker do
  def strict_check(user, request, facts) do
    request.authorization_steps
    |> Enum.reduce(facts, fn {_step, condition}, facts ->
      case Map.fetch(facts, {request.relationship, condition}) do
        {:ok, _boolean_result} ->
          facts

        :error ->
          case do_strict_check(condition, user, request) do
            :unknown ->
              facts

            :unknowable ->
              Map.put(facts, {request.relationship, condition}, :unknowable)

            boolean ->
              Map.put(facts, {request.relationship, condition}, boolean)
          end
      end
    end)
  end

  # TODO: Work on this. Gunna move it to checker
  def run_checks(scenarios, facts) do
    :all_facts_fetched
  end

  defp do_strict_check({module, opts}, user, request) do
    case module.strict_check(user, request, opts) do
      {:ok, boolean} when is_boolean(boolean) ->
        boolean

      {:ok, :unknown} ->
        cond do
          request.strict_access? ->
            # This means "we needed a fact that we have no way of getting"
            # Because the fact was needed in the `strict_check` step
            :unknowable

          Ash.Authorization.Check.defines_check?(module) ->
            :unknown

          true ->
            :unknowable
        end
    end
  end
end
