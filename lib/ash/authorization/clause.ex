defmodule Ash.Authorization.Clause do
  defstruct [:path, :resource, :source, :check_module, :check_opts, :filter]

  def new(_path, resource, {mod, opts}, source, filter \\ nil) do
    %__MODULE__{
      # path: path,
      source: source,
      resource: resource,
      check_module: mod,
      check_opts: opts,
      filter: filter
    }
  end

  # TODO: Should we for sure special case this? I see no reason not to.
  def put_new_fact(facts, _path, _resource, {Ash.Authorization.Clause.Static, _}, _) do
    facts
  end

  def put_new_fact(facts, path, resource, {mod, opts}, value, filter \\ nil) do
    Map.put(facts, new(path, resource, {mod, opts}, filter), value)
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

  defp is_matching_clause?(clause, clause), do: true

  defp is_matching_clause?(clause, other_clause)
       when is_boolean(clause) or is_boolean(other_clause),
       do: false

  defp is_matching_clause?(_, %__MODULE__{filter: nil}), do: true
  defp is_matching_clause?(%__MODULE__{filter: nil}, _), do: false

  defp is_matching_clause?(clause, potential_matching) do
    Map.put(clause, :filter, nil) == Map.put(potential_matching, :filter, nil) &&
      Ash.Filter.strict_subset_of?(potential_matching.filter, clause.filter)
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

    source =
      case clause.source do
        :root ->
          ""

        source ->
          to_string(source)
      end

    terminator =
      if filter != "" || source != "" do
        ": "
      else
        ""
      end

    concat([
      "#Clause<",
      source,
      filter,
      terminator,
      to_doc(clause.check_module.describe(clause.check_opts), opts),
      ">"
    ])
  end
end
