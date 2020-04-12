defmodule Ash.Authorization.Clause do
  defstruct [:path, :resource, :check_module, :check_opts, :filter]

  def new(resource, {mod, opts}, filter \\ nil) do
    %__MODULE__{
      resource: resource,
      check_module: mod,
      check_opts: opts,
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

  def expression(clause = %{filter: nil}) do
    clause
  end

  def expression(clause) do
    {:or, clause, %{clause | filter: nil}}
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

  defp is_matching_clause?(clause, %__MODULE__{filter: nil} = potential_matching) do
    Map.take(clause, [:resource, :check_module, :check_opts]) ==
      Map.take(potential_matching, [:resource, :check_module, :check_opts])
  end

  defp is_matching_clause?(%__MODULE__{filter: nil}, _), do: false

  defp is_matching_clause?(clause, potential_matching) do
    Ash.Filter.strict_subset_of?(potential_matching.filter, clause.filter) &&
      Map.take(clause, [:resource, :check_module, :check_opts]) ==
        Map.take(potential_matching, [:resource, :check_module, :check_opts])
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
      filter,
      terminator,
      to_doc(clause.check_module.describe(clause.check_opts), opts),
      ">"
    ])
  end
end
