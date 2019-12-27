defmodule Ash.Error.Forbidden do
  @moduledoc "Raised when authorization for an action fails"
  defexception [:scenarios, :authorization_steps, :facts, :strict_check_facts]
  # TODO: Use better logic to format this

  def message(error) do
    header = "forbidden:\n\n"
    explained_steps = explain_steps(error.authorization_steps, error.facts)

    explained_facts = explain_facts(error.facts, error.strict_check_facts || %{})

    main_message =
      header <>
        "Facts Gathered\n" <>
        indent(explained_facts) <> "\n\nAuthorization Steps:\n" <> indent(explained_steps)

    main_message <> "\n\nScenarios:\n" <> indent(explain_scenarios(error.scenarios))
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

  defp explain_facts(facts, strict_check_facts) do
    facts
    |> Map.drop([true, false])
    |> Enum.map_join("\n", fn {{rel, {mod, opts}}, status} ->
      gets_star? =
        Map.fetch(strict_check_facts, {rel, {mod, opts}}) in [
          {:ok, true},
          {:ok, false}
        ]

      star =
        if gets_star? do
          " ⭑"
        else
          ""
        end

      if rel == [] do
        status_to_mark(status) <> " " <> mod.describe(opts) <> star
      else
        status_to_mark(status) <> " " <> Enum.join(rel, ".") <> " " <> mod.describe(opts) <> star
      end
    end)
  end

  defp status_to_mark(true), do: "✓"
  defp status_to_mark(false), do: "✗"
  defp status_to_mark(:unknowable), do: "!"
  defp status_to_mark(nil), do: "?"

  defp indent(string) do
    string
    |> String.split("\n")
    |> Enum.map(fn line -> "  " <> line end)
    |> Enum.join("\n")
  end

  defp explain_steps(sets_of_authorization_steps, facts) do
    Enum.map_join(sets_of_authorization_steps, "---", fn authorization_steps ->
      authorization_steps
      |> Enum.map(fn {step, {relationship, {mod, opts}}} ->
        status_mark = status_to_mark(Map.get(facts, {relationship, {mod, opts}}))

        mark =
          status_mark <> " " <> step_to_mark(step, Map.get(facts, {relationship, {mod, opts}}))

        if relationship == [] do
          mark <> " | " <> to_string(step) <> ": " <> mod.describe(opts)
        else
          mark <>
            " | " <>
            to_string(step) <>
            ": #{Enum.join(relationship, ".")} " <> mod.describe(opts)
        end
      end)
      |> Enum.join("\n")
    end)
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
