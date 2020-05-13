defmodule Ash.Authorization.Report do
  alias Ash.Authorization.Clause
  alias Ash.Engine.Request

  defstruct [
    :scenarios,
    :requests,
    :facts,
    :state,
    :header,
    :authorized?,
    :api,
    :reason,
    path: [],
    no_steps_configured: false
  ]

  def report_from_engine(engine) do
    report(%__MODULE__{
      scenarios: engine.scenarios,
      requests: engine.requests,
      facts: engine.facts,
      authorized?: engine.authorized?,
      state: engine.data,
      api: engine.api
    })
  end

  def report(%{no_steps_configured: %Ash.Engine.Request{} = request}) do
    "forbidden:\n" <>
      request.name <> ": no authorization steps configured. Resource: #{request.resource}"
  end

  # We know that each group of authorization steps shares the same relationship
  def report(report) do
    header = (report.header || "Authorization Report") <> "\n"

    header =
      if report.path do
        Enum.join(report.path, ".") <> " " <> header
      else
        header
      end

    facts = Ash.Authorization.Clause.prune_facts(report.facts)

    explained_facts = explain_facts(facts)

    reason =
      if report.reason do
        "\n" <> report.reason <> "\n"
      else
        ""
      end

    explained_steps =
      Enum.map_join(report.requests, "\n", fn request ->
        header =
          if request.strict_access? do
            request.name <> "(strict access)"
          else
            request.name
          end

        header <> "\n" <> indent(explain_steps([request], report.api, facts)) <> "\n"
      end)

    main_message =
      header <>
        reason <>
        indent("Facts Gathered\n" <> indent(explained_facts) <> "\n\n" <> explained_steps)

    if report.authorized? do
      main_message
    else
      main_message <> indent("\n\nScenarios:\n" <> indent(explain_scenarios(report.scenarios)))
    end
  end

  defp explain_scenarios(scenarios) when scenarios in [nil, []] do
    """
    No scenarios found. Under construction.
    Eventually, scenarios will explain what data you could change to make the request possible.
    """
  end

  defp explain_scenarios(scenarios) do
    """
    #{Enum.count(scenarios)} found. Under construction.
    Eventually, scenarios will explain what data you could change to make the request possible.
    """
  end

  defp explain_facts(facts) when facts == %{true => true, false => false},
    do: "No facts gathered."

  defp explain_facts(facts) do
    facts
    |> Map.drop([true, false])
    |> Enum.map(fn {%{filter: filter} = key, value} ->
      {key, value, Ash.Filter.count_of_clauses(filter)}
    end)
    # TODO: nest child filters under parent filters?
    |> Enum.group_by(fn {clause, _value, _count_of_clauses} ->
      if clause.filter do
        clause.filter.resource
      else
        nil
      end
    end)
    |> Enum.map_join("\n", fn {resource, clauses} ->
      clauses =
        Enum.sort_by(clauses, fn {_, _, count_of_clauses} ->
          count_of_clauses
        end)

      explained =
        Enum.map_join(clauses, "\n", fn {clause, value, count_of_clauses} ->
          cond do
            clause.request_id ->
              inspect(clause.request_id) <>
                ": " <>
                clause.check_module.describe(clause.check_opts) <> " " <> status_to_mark(value)

            count_of_clauses == 0 ->
              clause.check_module.describe(clause.check_opts) <> " " <> status_to_mark(value)

            true ->
              inspect(clause.filter) <>
                ": " <>
                clause.check_module.describe(clause.check_opts) <> " " <> status_to_mark(value)
          end
        end)

      if resource do
        "#{inspect(resource)}: \n" <> indent(explained) <> "\n"
      else
        explained <> "\n"
      end
    end)
  end

  defp status_to_mark(true), do: "✓"
  defp status_to_mark(false), do: "✗"
  defp status_to_mark(:unknowable), do: "?"
  defp status_to_mark(:irrelevant), do: "⊘"
  defp status_to_mark(nil), do: "-"

  defp indent(string) do
    string
    |> String.split("\n")
    |> Enum.map(fn line -> "  " <> line end)
    |> Enum.join("\n")
  end

  defp explain_steps(requests, api, facts) do
    requests_with_data_filter =
      Enum.flat_map(requests, fn request ->
        if Request.data_resolved?(request) && request.data not in [nil, []] do
          request.data
          |> List.wrap()
          |> Enum.map(fn item ->
            %{request | query: Ash.Engine.get_pkeys(request, api, item)}
          end)
        else
          [request]
        end
      end)

    contents =
      requests_with_data_filter
      |> Enum.sort_by(fn request -> Enum.count(request.path) end)
      |> Enum.map_join("\n------\n", fn request ->
        contents =
          request.rules
          |> Enum.map(fn {step, clause} ->
            status =
              case Clause.find(facts, Map.put(clause, :filter, request.query.filter)) do
                {:ok, value} ->
                  value

                _ ->
                  nil
              end

            status_mark = status_to_mark(status)
            step_mark = step_to_mark(step, status)

            mod = clause.check_module
            opts = clause.check_opts
            relationship = clause.path

            if relationship == [] do
              step_mark <>
                " | " <> to_string(step) <> ": " <> mod.describe(opts) <> " " <> status_mark
            else
              step_mark <>
                " | " <>
                to_string(step) <>
                ": #{Enum.join(relationship || [], ".")}: " <>
                mod.describe(opts) <> " " <> status_mark
            end
          end)
          |> Enum.join("\n")

        if request.action_type == :read do
          inspect(request.query.filter) <> "\n\n" <> contents
        else
          inspect(request.id) <> "\n\n" <> contents
        end
      end)

    contents
  end

  defp step_to_mark(:authorize_if, true), do: "✓"
  defp step_to_mark(:authorize_if, false), do: "↓"
  defp step_to_mark(:authorize_if, _), do: "↓"

  defp step_to_mark(:forbid_if, true), do: "✗"
  defp step_to_mark(:forbid_if, false), do: "↓"
  defp step_to_mark(:forbid_if, _), do: "✗"

  defp step_to_mark(:authorize_unless, true), do: "↓"
  defp step_to_mark(:authorize_unless, false), do: "✓"
  defp step_to_mark(:authorize_unless, _), do: "↓"

  defp step_to_mark(:forbid_unless, true), do: "↓"
  defp step_to_mark(:forbid_unless, false), do: "✗"
  defp step_to_mark(:forbid_unless, _), do: "✗"
end
