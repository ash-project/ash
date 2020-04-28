defmodule Ash.Authorization.Clause do
  defstruct [:path, :resource, :request_id, :check_module, :check_opts, :action, :filter]

  def new(resource, {mod, opts}, action, filter, request_id \\ nil) do
    # Read actions should pass in `read` here,
    # once we have custom actions
    %__MODULE__{
      resource: resource,
      check_module: mod,
      check_opts: opts,
      action: action,
      request_id: request_id,
      filter: filter
    }
  end

  def find(_clauses, %{check_module: Ash.Authorization.Check.Static, check_opts: check_opts}) do
    {:ok, check_opts[:result]}
  end

  def find(clauses, clause) do
    Enum.find_value(clauses, fn {key, value} ->
      if is_matching_clause?(key, clause) do
        {:ok, value}
      end
    end) || :error
  end

  def prune_facts(facts) do
    new_facts = do_prune_facts(facts)

    if new_facts == facts do
      new_facts
    else
      do_prune_facts(new_facts)
    end
  end

  defp do_prune_facts(facts) do
    Enum.reduce(facts, facts, fn {clause, _value}, facts ->
      without_clause = Map.delete(facts, clause)

      case find(without_clause, clause) do
        nil ->
          without_clause

        _ ->
          facts
      end
    end)
  end

  defp is_matching_clause?(clause, clause), do: true

  defp is_matching_clause?(clause, other_clause)
       when is_boolean(clause) or is_boolean(other_clause),
       do: false

  defp is_matching_clause?(clause, potential_matching) do
    cond do
      clause.check_module.pure? ->
        match_keys = [:check_module, :check_opts]

        Map.take(clause, match_keys) ==
          Map.take(potential_matching, match_keys)

      clause.request_id ->
        match_keys = [:resource, :check_module, :check_opts, :request_id]

        Map.take(clause, match_keys) == Map.take(potential_matching, match_keys)

      potential_matching.request_id ->
        false

      true ->
        match_keys = [:resource, :check_module, :check_opts]

        Map.take(clause, match_keys) == Map.take(potential_matching, match_keys) and
          Ash.Filter.strict_subset_of?(clause.filter, potential_matching.filter)
    end
  end
end

defimpl Inspect, for: Ash.Authorization.Clause do
  import Inspect.Algebra

  def inspect(clause, opts) do
    filter =
      if clause.filter do
        concat(["(", to_doc(clause.filter, opts), ")"])
      else
        ""
      end

    terminator =
      if filter != "" do
        ": "
      else
        ""
      end

    concat([
      "#Clause<",
      inspect(clause.resource),
      ": ",
      filter,
      terminator,
      to_doc(clause.check_module.describe(clause.check_opts), opts),
      ">"
    ])
  end
end
