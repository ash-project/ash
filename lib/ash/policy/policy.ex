defmodule Ash.Policy.Policy do
  @moduledoc "Represents a policy on an Ash.Resource"

  # For now we just write to `checks` and move them to `policies`
  # on build, when we support nested policies we can change that.
  defstruct [
    :condition,
    :policies,
    :bypass?,
    :checks,
    :description,
    :access_type
  ]

  @type t :: %__MODULE__{}

  @static_checks [
    {Ash.Policy.Check.Static, [result: true]},
    {Ash.Policy.Check.Static, [result: false]},
    true,
    false
  ]

  def solve(authorizer) do
    authorizer.policies
    |> build_requirements_expression(authorizer)
    |> case do
      {true, authorizer} ->
        {:ok, true, authorizer}

      {false, authorizer} ->
        {:ok, false, authorizer}

      {expression, authorizer} ->
        Ash.Policy.SatSolver.solve(expression, fn scenario, bindings ->
          scenario
          |> Ash.SatSolver.solutions_to_predicate_values(bindings)
          |> Map.drop(@static_checks)
        end)
        |> case do
          {:ok, scenarios} ->
            {:ok, scenarios, authorizer}

          {:error, error} ->
            {:error, authorizer, error}
        end
    end
  catch
    {:error, authorizer, error} ->
      {:error, authorizer, error}
  end

  @doc false
  def transform(policy) do
    if Enum.empty?(policy.policies) do
      {:error, "Policies must have at least one check."}
    else
      {:ok, policy}
    end
  end

  defp build_requirements_expression(policies, authorizer) do
    {at_least_one_policy_expression, authorizer} =
      at_least_one_policy_expression(policies, authorizer)

    if at_least_one_policy_expression == false do
      {false, authorizer}
    else
      {policy_expression, authorizer} =
        if at_least_one_policy_expression == true do
          compile_policy_expression(policies, authorizer)
        else
          {policy_expression, authorizer} = compile_policy_expression(policies, authorizer)

          case {:and, at_least_one_policy_expression, policy_expression} do
            {:and, false, _} ->
              {false, authorizer}

            {:and, _, false} ->
              {false, authorizer}

            {:and, true, true} ->
              {true, authorizer}

            {:and, left, true} ->
              {left, authorizer}

            {:and, true, right} ->
              {right, authorizer}

            other ->
              {other, authorizer}
          end
        end

      facts_expression =
        authorizer.facts
        |> Map.drop([true, false])
        |> Ash.Policy.SatSolver.facts_to_statement()

      if facts_expression do
        {{:and, facts_expression, policy_expression}, authorizer}
      else
        {policy_expression, authorizer}
      end
    end
  end

  def at_least_one_policy_expression(policies, authorizer) do
    policies
    |> List.wrap()
    |> Enum.reduce({[], authorizer}, fn
      policy, {condition_exprs, authorizer} when is_list(condition_exprs) ->
        case condition_expression(policy.condition, authorizer) do
          {nil, authorizer} ->
            {condition_exprs, authorizer}

          {true, authorizer} ->
            {true, authorizer}

          {condition_expr, authorizer} ->
            {[condition_expr | condition_exprs], authorizer}
        end

      _, {true, authorizer} ->
        {true, authorizer}
    end)
    |> then(fn
      {true, authorizer} ->
        {true, authorizer}

      {condition_exprs, authorizer} ->
        {condition_exprs
         |> Enum.reduce(false, fn
           _, true ->
             true

           true, _ ->
             true

           false, acc ->
             acc

           condition, acc ->
             {:or, condition, acc}
         end), authorizer}
    end)
  end

  def fetch_or_strict_check_fact(authorizer, %{check_module: mod, check_opts: opts}) do
    fetch_or_strict_check_fact(authorizer, {mod, opts})
  end

  def fetch_or_strict_check_fact(authorizer, {Ash.Policy.Check.Static, opts}) do
    {:ok, opts[:result], authorizer}
  end

  def fetch_or_strict_check_fact(authorizer, {check_module, opts}) do
    Enum.find_value(authorizer.facts, fn
      {{fact_mod, fact_opts}, result} ->
        if check_module == fact_mod &&
             Keyword.drop(fact_opts, [:access_type, :ash_field_policy?]) ==
               Keyword.drop(opts, [:access_type, :ash_field_policy?]) do
          {:ok, result}
        end

      _ ->
        nil
    end)
    |> case do
      nil ->
        if check_module.requires_original_data?(authorizer, opts) &&
             missing_original_data?(authorizer) do
          throw(
            {:error, authorizer,
             Ash.Error.Forbidden.InitialDataRequired.exception(
               source: "check: #{check_module.describe(opts)} requires initial data"
             )}
          )
        else
          case check_module.strict_check(authorizer.actor, authorizer, opts) do
            {:ok, value} when is_boolean(value) or value == :unknown ->
              authorizer = %{
                authorizer
                | facts: Map.put(authorizer.facts, {check_module, opts}, value)
              }

              if value == :unknown do
                {:error, authorizer}
              else
                {:ok, value, authorizer}
              end

            {:error, error} ->
              throw({:error, authorizer, Ash.Error.to_ash_error(error)})
          end
        end

      {:ok, :unknown} ->
        {:error, authorizer}

      {:ok, value} ->
        {:ok, value, authorizer}
    end
  end

  defp missing_original_data?(%{
         changeset: %Ash.Changeset{data: %Ash.Changeset.OriginalDataNotAvailable{}}
       }) do
    true
  end

  defp missing_original_data?(_), do: false

  def fetch_fact(facts, %{check_module: mod, check_opts: opts}) do
    fetch_fact(facts, {mod, opts})
  end

  def fetch_fact(_facts, {Ash.Policy.Check.Static, opts}) do
    {:ok, opts[:result]}
  end

  def fetch_fact(facts, {mod, opts}) do
    Enum.find_value(facts, fn
      {{fact_mod, fact_opts}, result} ->
        if mod == fact_mod &&
             Keyword.drop(fact_opts, [:access_type, :ash_field_policy?]) ==
               Keyword.drop(opts, [:access_type, :ash_field_policy?]) do
          {:ok, result}
        end

      _ ->
        nil
    end)
    |> case do
      nil ->
        :error

      :unknown ->
        :error

      value ->
        value
    end
  end

  defp condition_expression(condition, authorizer) do
    condition
    |> List.wrap()
    |> Enum.reduce({nil, authorizer}, fn
      condition, {nil, authorizer} ->
        case fetch_or_strict_check_fact(authorizer, condition) do
          {:ok, true, authorizer} ->
            {true, authorizer}

          {:ok, false, authorizer} ->
            {false, authorizer}

          _ ->
            {condition, authorizer}
        end

      _condition, {false, authorizer} ->
        {false, authorizer}

      condition, {expression, authorizer} ->
        case fetch_or_strict_check_fact(authorizer, condition) do
          {:ok, true, authorizer} ->
            {expression, authorizer}

          {:ok, false, authorizer} ->
            {false, authorizer}

          _ ->
            {{:and, condition, expression}, authorizer}
        end
    end)
  end

  defp compile_policy_expression([], authorizer) do
    {false, authorizer}
  end

  defp compile_policy_expression(
         [%struct{condition: condition, policies: policies}],
         authorizer
       )
       when struct in [__MODULE__, Ash.Policy.FieldPolicy] do
    {condition_expression, authorizer} = condition_expression(condition, authorizer)

    case condition_expression do
      true ->
        compile_policy_expression(policies, authorizer)

      false ->
        {true, authorizer}

      nil ->
        compile_policy_expression(policies, authorizer)

      condition_expression ->
        case compile_policy_expression(policies, authorizer) do
          {true, authorizer} ->
            {true, authorizer}

          {false, authorizer} ->
            {{:not, condition_expression}, authorizer}

          {compiled_policies, authorizer} ->
            {{:or, {:and, condition_expression, compiled_policies}, {:not, condition_expression}},
             authorizer}
        end
    end
  end

  defp compile_policy_expression(
         [
           %{condition: condition, policies: policies, bypass?: bypass?} | rest
         ],
         authorizer
       ) do
    {condition_expression, authorizer} = condition_expression(condition, authorizer)

    case condition_expression do
      empty when empty in [nil, true] ->
        if bypass? do
          case compile_policy_expression(policies, authorizer) do
            {true, authorizer} ->
              {true, authorizer}

            {false, authorizer} ->
              {at_least_one_policy_expression, authorizer} =
                at_least_one_policy_expression(Enum.take_while(rest, &(!&1.bypass?)), authorizer)

              {rest, authorizer} = compile_policy_expression(rest, authorizer)

              {{:and, rest, at_least_one_policy_expression}, authorizer}

            {policy_expression, authorizer} ->
              {at_least_one_policy_expression, authorizer} =
                at_least_one_policy_expression(Enum.take_while(rest, &(!&1.bypass?)), authorizer)

              {rest, authorizer} = compile_policy_expression(rest, authorizer)
              {{:or, policy_expression, {:and, rest, at_least_one_policy_expression}}, authorizer}
          end
        else
          case compile_policy_expression(policies, authorizer) do
            {false, authorizer} ->
              {false, authorizer}

            {true, authorizer} ->
              compile_policy_expression(rest, authorizer)

            {policy_expression, authorizer} ->
              case compile_policy_expression(rest, authorizer) do
                {false, authorizer} ->
                  {false, authorizer}

                {true, authorizer} ->
                  {policy_expression, authorizer}

                {rest, authorizer} ->
                  {{:and, policy_expression, rest}, authorizer}
              end
          end
        end

      false ->
        compile_policy_expression(rest, authorizer)

      condition_expression ->
        if bypass? do
          {at_least_one_policy_expression, authorizer} =
            at_least_one_policy_expression(Enum.take_while(rest, &(!&1.bypass?)), authorizer)

          {condition_and_policy_expression, authorizer} =
            case compile_policy_expression(policies, authorizer) do
              {true, authorizer} ->
                {condition_expression, authorizer}

              {false, authorizer} ->
                {false, authorizer}

              {other, authorizer} ->
                {{:and, condition_expression, other}, authorizer}
            end

          case condition_and_policy_expression do
            true ->
              {true, authorizer}

            false ->
              case compile_policy_expression(rest, authorizer) do
                {true, authorizer} ->
                  {at_least_one_policy_expression, authorizer}

                {false, authorizer} ->
                  {false, authorizer}

                {rest, authorizer} ->
                  {{:and, rest, at_least_one_policy_expression}, authorizer}
              end

            condition_and_policy_expression ->
              case compile_policy_expression(rest, authorizer) do
                {true, authorizer} ->
                  {at_least_one_policy_expression, authorizer}

                {false, authorizer} ->
                  {condition_and_policy_expression, authorizer}

                {rest, authorizer} ->
                  {{:or, condition_and_policy_expression,
                    {:and, rest, at_least_one_policy_expression}}, authorizer}
              end
          end
        else
          case compile_policy_expression(policies, authorizer) do
            {true, authorizer} ->
              compile_policy_expression(rest, authorizer)

            {false, authorizer} ->
              {rest_expr, authorizer} = compile_policy_expression(rest, authorizer)

              {{:and, {:not, condition_expression}, rest_expr}, authorizer}

            {policy_expression, authorizer} ->
              {rest_expr, authorizer} = compile_policy_expression(rest, authorizer)

              {{:and,
                {:or, {:not, condition_expression},
                 {:and, condition_expression, policy_expression}}, rest_expr}, authorizer}
          end
        end
    end
  end

  defp compile_policy_expression(
         [%{type: :authorize_if} = clause],
         authorizer
       ) do
    case fetch_or_strict_check_fact(authorizer, clause) do
      {:ok, true, authorizer} ->
        {true, authorizer}

      {:ok, false, authorizer} ->
        {false, authorizer}

      {:error, authorizer} ->
        {{clause.check_module, clause.check_opts}, authorizer}
    end
  end

  defp compile_policy_expression(
         [%{type: :authorize_if} = clause | rest],
         authorizer
       ) do
    case fetch_or_strict_check_fact(authorizer, clause) do
      {:ok, true, authorizer} ->
        {true, authorizer}

      {:ok, false, authorizer} ->
        compile_policy_expression(rest, authorizer)

      {:error, authorizer} ->
        {rest, authorizer} = compile_policy_expression(rest, authorizer)
        {{:or, {clause.check_module, clause.check_opts}, rest}, authorizer}
    end
  end

  defp compile_policy_expression(
         [%{type: :authorize_unless} = clause],
         authorizer
       ) do
    case fetch_or_strict_check_fact(authorizer, clause) do
      {:ok, true, authorizer} ->
        {false, authorizer}

      {:ok, false, authorizer} ->
        {true, authorizer}

      {:error, authorizer} ->
        {{:not, {clause.check_module, clause.check_opts}}, authorizer}
    end
  end

  defp compile_policy_expression(
         [%{type: :authorize_unless} = clause | rest],
         authorizer
       ) do
    case fetch_or_strict_check_fact(authorizer, clause) do
      {:ok, true, authorizer} ->
        compile_policy_expression(rest, authorizer)

      {:ok, false, authorizer} ->
        {true, authorizer}

      {:error, authorizer} ->
        {rest, authorizer} = compile_policy_expression(rest, authorizer)
        {{:or, {:not, {clause.check_module, clause.check_opts}}, rest}, authorizer}
    end
  end

  defp compile_policy_expression([%{type: :forbid_if}], authorizer) do
    {false, authorizer}
  end

  defp compile_policy_expression(
         [%{type: :forbid_if} = clause | rest],
         authorizer
       ) do
    case fetch_or_strict_check_fact(authorizer, clause) do
      {:ok, true, authorizer} ->
        {false, authorizer}

      {:ok, false, authorizer} ->
        compile_policy_expression(rest, authorizer)

      {:error, authorizer} ->
        {rest, authorizer} = compile_policy_expression(rest, authorizer)

        case rest do
          true ->
            {{:not, {clause.check_module, clause.check_opts}}, authorizer}

          false ->
            {false, authorizer}

          rest ->
            {{:and, {:not, {clause.check_module, clause.check_opts}}, rest}, authorizer}
        end
    end
  end

  defp compile_policy_expression([%{type: :forbid_unless}], authorizer) do
    {false, authorizer}
  end

  defp compile_policy_expression(
         [%{type: :forbid_unless} = clause | rest],
         authorizer
       ) do
    case fetch_or_strict_check_fact(authorizer, clause) do
      {:ok, true, authorizer} ->
        compile_policy_expression(rest, authorizer)

      {:ok, false, authorizer} ->
        {false, authorizer}

      {:error, authorizer} ->
        {rest, authorizer} = compile_policy_expression(rest, authorizer)

        case rest do
          false ->
            {false, authorizer}

          true ->
            {{clause.check_module, clause.check_opts}, authorizer}

          rest ->
            {{:and, {clause.check_module, clause.check_opts}, rest}, authorizer}
        end
    end
  end
end
