defmodule Ash.Authorization.Checker do
  @moduledoc """
  Determines if a set of authorization requests can be met or not.

  To read more about boolean satisfiability, see this page:
  https://en.wikipedia.org/wiki/Boolean_satisfiability_problem. At the end of
  the day, however, it is not necessary to understand exactly how Ash takes your
  authorization requirements and determines if a request is allowed. The
  important thing to understand is that Ash may or may not run any/all of your
  authorization rules as they may be deemed unnecessary. As such, authorization
  checks should have no side effects. Ideally, the checks built-in to ash should
  cover the bulk of your needs.

  If you need to write your own checks see #TODO: Link to a guide about writing checks here.
  """

  alias Ash.Authorization.Clause

  def strict_check(user, request, facts) do
    new_facts =
      request.rules
      |> Enum.reduce(facts, fn {_step, clause}, facts ->
        case Clause.find(facts, clause) do
          {:ok, _boolean_result} ->
            facts

          :error ->
            case do_strict_check(clause, user, request) do
              {:error, _error} ->
                # TODO: Surface this error
                facts

              :unknown ->
                facts

              :unknowable ->
                Map.put(facts, clause, :unknowable)

              :irrelevant ->
                Map.put(facts, clause, :irrelevant)

              boolean ->
                Map.put(facts, clause, boolean)
            end
        end
      end)

    {Map.put(request, :strict_check_complete?, true), new_facts}
  end

  def run_checks(engine, %{data: []}, _clause) do
    {:ok, engine}
  end

  def run_checks(engine, request, clause) do
    case clause.check_module().check(engine.user, request.data, %{}, clause.check_opts) do
      {:error, error} ->
        {:error, error}

      {:ok, check_result} ->
        pkey = Ash.primary_key(request.resource)

        {authorized, unauthorized} =
          Enum.split_with(request.data, fn data ->
            data_pkey = Map.take(data, pkey)

            Enum.find(check_result, fn authorized ->
              Map.take(authorized, pkey) == data_pkey
            end)
          end)

        case {authorized, unauthorized} do
          {_, []} ->
            {:ok, %{engine | facts: Map.put(engine.facts, clause, true)}}

          {[], _} ->
            {:ok, %{engine | facts: Map.put(engine.facts, clause, false)}}

          {authorized, unauthorized} ->
            # TODO: Handle this error
            {:ok, authorized_values} =
              Ash.Actions.PrimaryKeyHelpers.values_to_primary_key_filters(
                request.resource,
                authorized
              )

            authorized_filter =
              Ash.Filter.parse(request.resource, [or: authorized_values], engine.api)

            {:ok, unauthorized_values} =
              Ash.Actions.PrimaryKeyHelpers.values_to_primary_key_filters(
                request.resource,
                unauthorized
              )

            unauthorized_filter =
              Ash.Filter.parse(request.resource, [or: unauthorized_values], engine.api)

            authorized_clause = %{clause | filter: authorized_filter}
            unauthorized_clause = %{clause | filter: unauthorized_filter}

            new_facts =
              engine.facts
              |> Map.delete(clause)
              |> Map.put(authorized_clause, true)
              |> Map.put(unauthorized_clause, false)

            {:ok, %{engine | facts: new_facts}}
        end
    end
  end

  defp do_strict_check(%{check_module: module, check_opts: opts}, user, request) do
    case module.strict_check(user, request, opts) do
      {:error, error} ->
        {:error, error}

      {:ok, boolean} when is_boolean(boolean) ->
        boolean

      {:ok, :irrelevant} ->
        :irrelevant

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
