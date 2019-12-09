defmodule Ash.Authorization.Authorizer do
  alias Ash.Authorization.Rule

  @type result :: :allow | :unauthorized | :undecided

  def authorize(user, rules, context, callback) do
    case authorize_precheck(user, rules, context) do
      {%{prediction: :unknown}, per_check_data} ->
        callback.(fn user, data, rules, context ->
          do_authorize(user, data, rules, context, per_check_data)
        end)
    end
  end

  defp authorize_precheck(user, rules, context) do
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
  defp do_authorize(user, data, rules, context, per_check_data) do
    {_decision, remaining_records} =
      rules
      |> Enum.zip(per_check_data)
      |> Enum.reduce({:undecided, data}, fn
        {rule, per_check_data}, {:undecided, data} ->
          rule_with_per_check_data =
            case per_check_data do
              %{decision: value} ->
                %{rule | check: fn _, _, _ -> value end}

              _ ->
                rule
            end

          full_context = Map.merge(context, Map.get(per_check_data, :context) || %{})

          checked_records = run_check(rule_with_per_check_data, user, data, full_context)

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

  defp run_check(
         %{check: check, kind: kind},
         user,
         data,
         context
       ) do
    check_function =
      case check do
        {module, function, args} ->
          fn user, data, context ->
            apply(module, function, [user, data, context] ++ args)
          end

        function ->
          function
      end

    result = check_function.(user, data, context)

    Enum.map(data, fn item ->
      result =
        case result do
          true -> true
          false -> false
          ids -> item.id in ids
        end

      decision = Rule.result_to_decision(kind, result)
      Map.put(item, :__authorization_decision__, decision)
    end)
  end

  defp predict_result({instructions, per_check_data}, rules) do
    prediction = get_prediction(Enum.zip(rules, per_check_data))

    {Map.put(instructions, :prediction, prediction), per_check_data}
  end

  defp get_prediction([]), do: :unknown

  defp get_prediction([{rule, %{decision: value}} | rest]) do
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
  defp handle_precheck_result(:ok, instructions_and_data), do: instructions_and_data

  defp handle_precheck_result({:context, context}, {instructions, data}) do
    {instructions, Map.update(data, :context, context, &Map.merge(&1, context))}
  end

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

  defp precheck_result(%{decision: nil}, _user, _context), do: nil

  defp precheck_result(%{decision: precheck}, user, context) do
    case precheck do
      {module, function, args} ->
        if function_exported?(module, function, Enum.count(args) + 2) do
          apply(module, function, [user, context] ++ args)
        else
          nil
        end

      function ->
        function.(user, context)
    end
  end
end
