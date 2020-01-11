defmodule Ash.Authorization.Clause do
  defstruct [:relationship, :resource, :check_module, :check_opts, :pkey]

  def new(rel, resource, {mod, opts}, pkey \\ nil) do
    %__MODULE__{
      relationship: rel,
      resource: resource,
      check_module: mod,
      check_opts: opts,
      pkey: pkey
    }
  end

  # TODO: Should we for sure special case this? I see no reason not to.
  def put_new_fact(facts, _rel, _resource, {Ash.Authorization.Clause.Static, _}, _) do
    facts
  end

  def put_new_fact(facts, rel, resource, {mod, opts}, value, pkey \\ nil) do
    Map.put(facts, new(rel, resource, {mod, opts}, pkey), value)
  end

  def find(_clauses, %{check_module: Ash.Authorization.Check.Static, check_opts: check_opts}) do
    {:ok, check_opts[:result]}
  end

  def find(clauses, clause) do
    case Map.fetch(clauses, %{clause | pkey: nil}) do
      {:ok, value} ->
        {:ok, value}

      :error ->
        Map.fetch(clauses, clause)
    end
  end

  def expression(clause = %{pkey: nil}) do
    clause
  end

  def expression(clause) do
    {:or, clause, %{clause | pkey: nil}}
  end
end
