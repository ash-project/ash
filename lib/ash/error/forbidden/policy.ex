defmodule Ash.Error.Forbidden.Policy do
  @moduledoc "Raised when policy authorization for an action fails"

  require Logger
  use Ash.Error.Exception

  alias Ash.Policy.Policy

  use Splode.Error,
    fields: [
      scenarios: [],
      facts: %{},
      filter: nil,
      policy_breakdown?: false,
      must_pass_strict_check?: false,
      for_fields: nil,
      context_description: nil,
      policies: [],
      resource: nil,
      action: nil,
      changeset_doesnt_match_filter: false
    ],
    class: :forbidden

  def exception(opts) do
    exception =
      super(Keyword.put(opts, :policy_breakdown?, Ash.Policy.Info.show_policy_breakdowns?()))

    case Ash.Policy.Info.log_policy_breakdowns() do
      nil ->
        :ok

      level ->
        Logger.log(level, report(exception, help_text?: false))
    end

    exception
  end

  def message(error) do
    if error.policy_breakdown? do
      "forbidden:\n\n#{Ash.Error.Forbidden.Policy.report(error, help_text?: false)}"
    else
      "forbidden"
    end
  end

  @help_text """

  A check status of `?` implies that the solver did not need to determine that check.
  Some checks may look like they failed when in reality there was no need to check them.
  Look for policies with `✘` and `✓` in check statuses.

  A check with a `⬇` means that it didn't determine if the policy was authorized or forbidden, and so moved on to the next check.
  `🌟` and `⛔` mean that the check was responsible for producing an authorized or forbidden (respectively) status.

  When viewing successful authorization breakdowns, a `🔎` means that the policy or check was enforced via a filter.

  If no check results in a status (they all have `⬇`) then the policy is assumed to have failed. In some cases, however, the policy
  may have just been ignored, as described above.
  """

  @doc """
  Print a report of an authorization failure from a forbidden error

  Options:

  - `:help_text?`: Defaults to true. Displays help text at the top of the policy breakdown.
  """
  def report(error, opts \\ []) do
    error
    |> get_errors()
    |> case do
      [] ->
        "No policy errors"

      errors ->
        error_lines =
          errors
          |> Enum.map(fn
            %{
              facts: facts,
              filter: filter,
              policies: policies,
              must_pass_strict_check?: must_pass_strict_check?
            } ->
              get_breakdown(
                facts,
                filter,
                policies,
                Keyword.merge(opts,
                  must_pass_strict_check?: must_pass_strict_check?,
                  context_description: error.context_description,
                  for_fields: error.for_fields
                )
              )
          end)
          |> Enum.intersperse("\n\n")

        [title_line(error), "\n", error_lines]
        |> IO.iodata_to_binary()
        |> String.trim()
    end
  end

  @doc """
  Print a report of an authorization failure from authorization information.

  Options:

  - `:help_text?`: Defaults to true. Displays help text at the top of the policy breakdown.
  - `:success?`: Defaults to false. Changes the messaging/graphics around to indicate successful policy authorization.
  - `:must_pass_strict_check?`: Defaults to false. Adds a message about this authorization requiring passing strict check.
  """
  def get_breakdown(facts, filter, policies, opts \\ []) do
    must_pass_strict_check? =
      if opts[:must_pass_strict_check?] && Enum.any?(policies, &(&1.access_type == :runtime)) do
        if Keyword.get(opts, :help_text?, true) do
          """

          Scenario must pass strict check only, meaning `runtime` policies cannot be checked.


          This requirement is generally used for filtering on related resources, when we can't fetch those
          related resources to run `runtime` policies. For this reason, you generally want your primary read
          actions on your resources to have standard policies which can be checked statically (like `actor_attribute_equals`)
          in addition to filter policies, like `expr(foo == :bar)`.
          """
        else
          """

          Scenario must pass strict check only, meaning `runtime` policies cannot be checked.
          """
        end
      else
        ""
      end

    policy_context_description =
      cond do
        opts[:for_fields] && opts[:context_description] ->
          " for #{opts[:context_description]} fields: #{inspect(opts[:for_fields])} "

        opts[:for_fields] ->
          " for fields: #{inspect(opts[:for_fields])} "

        opts[:context_description] ->
          " for #{opts[:context_description]}:"

        true ->
          ""
      end

    policy_breakdown_title =
      if Keyword.get(opts, :help_text?, true) do
        ["Policy Breakdown#{policy_context_description}", @help_text]
      else
        "Policy Breakdown#{policy_context_description}"
      end

    policy_explanation =
      policies
      |> Kernel.||([])
      |> Enum.filter(&relevant?(&1, facts))
      |> Enum.map(&explain_policy(&1, facts, opts[:success?] || false))
      |> Enum.intersperse("\n")
      |> title(policy_breakdown_title, false)

    filter =
      if filter do
        title(
          "#{nicely_formatted_filter(filter)}",
          "Generated Filter"
        )
      else
        ""
      end

    [must_pass_strict_check?, filter, policy_explanation]
    |> Enum.filter(& &1)
    |> Enum.intersperse("\n")
  end

  defp nicely_formatted_filter([{:or, list}]) when is_list(list) do
    "(" <> Enum.map_join(list, " or ", &nicely_formatted_filter/1) <> ")"
  end

  defp nicely_formatted_filter([{:and, list}]) when is_list(list) do
    "(" <> Enum.map_join(list, " and ", &nicely_formatted_filter/1) <> ")"
  end

  defp nicely_formatted_filter(value) do
    inspect(value)
  end

  defp title_line(error) do
    cond do
      error.resource && error.action ->
        "#{inspect(error.resource)}.#{action_name(error.action)}"

      error.resource ->
        "#{inspect(error.resource)}"

      error.action ->
        "#{action_name(error.action)}"
    end
  end

  defp action_name(%{name: name}), do: name
  defp action_name(name), do: name

  defp relevant?(policy, facts) do
    Enum.all?(policy.condition || [], fn condition ->
      Policy.fetch_fact(facts, condition) == {:ok, true}
    end)
  end

  defp title(other, title, semicolon \\ true)
  defp title([], _, _), do: []
  defp title(other, title, true), do: [title, ":\n", other]
  defp title(other, title, false), do: [title, "\n", other]

  defp explain_policy(policy, facts, success?) do
    bypass =
      if policy.bypass? do
        "Bypass: "
      else
        ""
      end

    {condition_description, applies} = describe_conditions(policy.condition, facts)

    if applies == true do
      {description, state} = describe_checks(policy.policies, facts, success?)

      tag =
        case state do
          :unknown ->
            # In successful cases, this means we must have filtered
            "🔎"

          :authorized ->
            "🌟"

          :forbidden ->
            "⛔"
        end

      title(
        [Enum.map(condition_description, &["    ", &1]), Enum.map(description, &["    ", &1])],
        [
          "  ",
          bypass,
          policy.description || "Policy",
          " | ",
          tag
        ]
      )
    else
      tag =
        if applies == false do
          "⬇️"
        else
          "?"
        end

      title(
        Enum.map(condition_description, &["    ", &1]),
        ["  ", bypass, policy.description || "Policy", " | ", tag]
      )
    end
  end

  defp describe_conditions(condition, facts) do
    condition
    |> List.wrap()
    |> case do
      [{Ash.Policy.Check.Static, opts}] ->
        {[], opts[:result]}

      conditions ->
        conditions
        |> Enum.reduce({[], true}, fn condition, {conditions, status} ->
          {mod, opts} =
            case condition do
              %{module: module, opts: opts} ->
                {module, opts}

              {module, opts} ->
                {module, opts}
            end

          new_status =
            if status in [false, :unknown] do
              false
            else
              case Policy.fetch_fact(facts, {mod, opts}) do
                {:ok, true} ->
                  true

                {:ok, false} ->
                  false

                _ ->
                  :unknown
              end
            end

          {[["condition: ", mod.describe(opts)] | conditions], new_status}
        end)
        |> then(fn {conditions, status} ->
          conditions =
            conditions
            |> Enum.reverse()
            |> case do
              [] ->
                []

              conditions ->
                [
                  conditions
                  |> Enum.intersperse("\n"),
                  "\n"
                ]
            end

          {conditions, status}
        end)
    end
  end

  defp describe_checks(checks, facts, success?) do
    {description, state} =
      Enum.reduce(checks, {[], :unknown}, fn check, {descriptions, state} ->
        new_state =
          case state do
            :unknown ->
              new_state(
                check.type,
                Policy.fetch_fact(facts, check.check)
              )

            other ->
              other
          end

        filter_check? = function_exported?(elem(check.check, 0), :auto_filter, 3)

        tag =
          case {state, new_state} do
            {:unknown, :authorized} ->
              "🌟"

            {:unknown, :forbidden} ->
              "⛔"

            {:unknown, :unknown} ->
              if (success? && filter_check? && Policy.fetch_fact(facts, check.check) == :error) ||
                   {:ok, :unknown} do
                "🔎"
              else
                "⬇"
              end

            _ ->
              ""
          end

        {[
           describe_check(
             check,
             Policy.fetch_fact(facts, check.check),
             tag,
             success?,
             filter_check?
           )
           | descriptions
         ], new_state}
      end)

    {Enum.intersperse(Enum.reverse(description), "\n"), state}
  end

  defp describe_check(check, fact_result, tag, success?, filter_check?) do
    fact_result =
      case fact_result do
        {:ok, true} ->
          "✓"

        {:ok, false} ->
          "✘"

        {:ok, :unknown} ->
          unknown_or_error_glyph(success?, filter_check?)

        :error ->
          unknown_or_error_glyph(success?, filter_check?)
      end

    [
      check_type(check),
      ": ",
      check.check_module.describe(check.check_opts),
      " | ",
      fact_result,
      " | ",
      tag
    ]
  end

  defp unknown_or_error_glyph(success?, filter_check?) when success? and filter_check?, do: "✓"
  defp unknown_or_error_glyph(_, _), do: "?"

  defp check_type(%{type: :authorize_if}), do: "authorize if"
  defp check_type(%{type: :forbid_if}), do: "forbid if"
  defp check_type(%{type: :authorize_unless}), do: "authorize unless"
  defp check_type(%{type: :forbid_unless}), do: "forbid unless"

  defp new_state(:authorize_if, {:ok, true}), do: :authorized
  defp new_state(:forbid_if, {:ok, true}), do: :forbidden
  defp new_state(:authorize_unless, {:ok, false}), do: :authorized
  defp new_state(:forbid_unless, {:ok, false}), do: :forbidden
  defp new_state(_, _), do: :unknown

  defp get_errors(%Ash.Error.Forbidden{errors: errors}) do
    Enum.flat_map(errors || [], fn error ->
      get_errors(error)
    end)
  end

  defp get_errors(%__MODULE__{} = error) do
    [error]
  end

  defp get_errors(_), do: []
end
