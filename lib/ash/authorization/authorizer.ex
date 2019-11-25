defmodule Ash.Authorization.Authorizer do
  alias Ash.Authorization.Rule

  def authorize_precheck(user, rules, context) do
    rules
    |> Enum.reduce({%{}, []}, fn rule, {instructions, per_check_data} ->
      {instructions, check_data} =
        rule
        |> precheck_result(user, context)
        |> List.wrap()
        |> Enum.reduce({instructions, %{}}, &handle_precheck_result/2)

      {instructions, [check_data | per_check_data]}
    end)
    |> predict_result(rules)
  end

  # Never call authorize w/o first calling authorize_precheck before
  # the operation
  def authorize(user, data, rules, context, per_check_data) do
    {_decision, remaining_records} =
      rules
      |> Enum.zip(per_check_data)
      |> Enum.reduce({:undecided, data}, fn
        {rule, per_check_data}, {:undecided, data} ->
          rule_with_per_check_data =
            case per_check_data do
              %{precheck: value} ->
                %{rule | check: fn _, _, _ -> value end}

              _ ->
                rule
            end

          checked_records = Rule.run_check(rule_with_per_check_data, user, data, context)

          if Enum.any?(checked_records, &(&1.__authorization_decision__ == :unauthorized)) do
            {:unauthorized, data}
          else
            remaining_records =
              Enum.reject(checked_records, &(&1.__authorization_decision__ == :allow))

            if Enum.empty?(remaining_records) do
              {:allow, []}
            else
              {:undecided, remaining_records}
            end
          end

        _, {decision, data} ->
          {decision, data}
      end)

    if Enum.empty?(remaining_records) do
      :allow
    else
      # Return some kind of information here?
      # Maybe full auth breakdown in dev envs?
      {:unauthorized, nil}
    end
  end

  defp predict_result({instructions, per_check_data}, rules) do
    prediction = get_prediction(Enum.zip(rules, per_check_data))

    {Map.put(instructions, :prediction, prediction), per_check_data}
  end

  defp get_prediction([]), do: :unknown

  defp get_prediction([{rule, %{precheck: value}} | rest]) do
    case Rule.result_to_decision(rule.kind, value) do
      :allow -> :allow
      :undecided -> get_prediction(rest)
      :unauthorized -> :unauthorized
    end
  end

  defp get_prediction([{rule, _} | rest]) do
    result_if_true = Rule.result_to_decision(rule.kind, true)
    result_if_false = Rule.result_to_decision(rule.kind, false)

    if result_if_true != :allow and result_if_false != :allow do
      :unauthorized
    else
      get_prediction(rest)
    end
  end

  defp handle_precheck_result(nil, instructions_and_data), do: instructions_and_data

  defp handle_precheck_result({:precheck, boolean}, {instructions, data})
       when is_boolean(boolean) do
    {instructions, Map.put(data, :precheck, boolean)}
  end

  defp handle_precheck_result({:side_load, relationship}, {instructions, data}) do
    new_instructions =
      instructions
      |> Map.put_new(:side_load, [])
      |> Map.update!(:side_load, &Keyword.put_new(&1, relationship, []))

    {new_instructions, data}
  end

  defp precheck_result(%{precheck: nil}, _user, _context), do: nil

  defp precheck_result(%{precheck: precheck, extra_context: extra_context}, user, context) do
    precheck.(user, Map.merge(extra_context, context))
  end
end
