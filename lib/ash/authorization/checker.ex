defmodule Ash.Authorization.Checker do
  alias Ash.Authorization.Clause

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
              Clause.put_new_fact(
                facts,
                request.relationship,
                request.resource,
                condition,
                :unknowable
              )

            boolean ->
              Clause.put_new_fact(
                facts,
                request.relationship,
                request.resource,
                condition,
                boolean
              )
          end
      end
    end)
  end

  # TODO: Work on this. Gunna move it to checker
  def run_checks(scenarios, facts, state) do
    scenarios
    |> Enum.flat_map(fn scenario ->
      scenario
      |> Map.drop([true, false])
      |> Enum.map(fn {clause, _value} ->
        clause
      end)
    end)
    |> Enum.uniq()
    |> choose_checks_to_run(facts, state)
    |> case do
      [] ->
        :all_scenarios_known

      clauses ->
        clauses
        |> prepare_checks(state)
        |> do_run_checks(facts)
    end
  end

  defp do_run_checks({clauses, %{data: data, user: user} = state}, facts) do
    Enum.reduce_while(clauses, {:ok, facts, state}, fn clause, {:ok, facts, state} ->
      mod = clause.check_module
      opts = clause.check_opts

      case mod.check(user, data, %{}, opts) do
        {:error, error} ->
          {:halt, {:error, error}}

        {:ok, []} ->
          {:cont, {:ok, Map.put(facts, clause, false), state}}

        {:ok, true} ->
          facts = Map.put(facts, clause, false)
          {:cont, {:ok, facts, state}}

        {:ok, false} ->
          facts = Map.put(facts, clause, false)
          {:cont, {:ok, facts, state}}

        {:ok, [%resource{} | _] = records} ->
          # TODO: Yet another thing that requires a primary key,
          # figure it out
          pkey = Ash.primary_key(resource)
          record_pkeys = Enum.map(records, &Map.take(&1, pkey))

          data
          |> Enum.split_with(fn record ->
            Map.take(record, pkey) in record_pkeys
          end)
          |> case do
            {[], _} ->
              facts = Map.put(facts, clause, false)
              {:cont, {:ok, facts, state}}

            {_, []} ->
              facts = Map.put(facts, clause, false)
              {:cont, {:ok, facts, state}}

            {true_data, false_data} ->
              facts = set_records_to(true_data, facts, clause, true, pkey)

              facts = set_records_to(false_data, facts, clause, false, pkey)

              {:cont, {:ok, facts, state}}
          end
      end
    end)
  end

  defp set_records_to(records, facts, clause, value, pkey) do
    Enum.reduce(records, facts, fn record, facts ->
      pkey_value = Map.take(record, pkey)

      Map.put(facts, %{clause | pkey: pkey_value}, value)
    end)
  end

  defp prepare_checks(clauses, state) do
    Enum.reduce(clauses, {[], state}, fn
      %{relationship: [], check_module: mod, check_opts: opts} = clause, {clauses, state} ->
        case mod.prepare(opts) do
          [] -> {[clause | clauses], state}
          _ -> raise "No preparations supported yet!"
        end

      _check, {_checks, _state} ->
        raise "Can't prepare/handle checks with a relationship"
    end)
  end

  # TODO: Get smart here
  defp choose_checks_to_run([], _, _), do: []

  defp choose_checks_to_run(checks, facts, _) do
    checks
    |> Enum.reject(fn check ->
      Clause.find(facts, check) != :error
    end)
    |> case do
      [] ->
        []

      checks ->
        [List.first(checks)]
    end
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
