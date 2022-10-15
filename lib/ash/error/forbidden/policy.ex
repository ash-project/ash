defmodule Ash.Error.Forbidden.Policy do
  @moduledoc "Raised when policy authorization for an action fails"

  require Logger
  use Ash.Error.Exception

  alias Ash.Policy.Policy

  def_ash_error(
    [
      scenarios: [],
      facts: %{},
      filter: nil,
      policy_breakdown?: false,
      must_pass_strict_check?: false,
      policies: [],
      resource: nil,
      action: nil,
      changeset_doesnt_match_filter: false
    ],
    class: :forbidden
  )

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

  @help_text """

  A check status of `?` implies that the solver did not need to determine that check.
  Some checks may look like they failed when in reality there was no need to check them.
  Look for policies with `✘` and `✓` in check statuses.

  A check with a `⬇` means that it didn't determine if the policy was authorized or forbidden, and so moved on to the next check.
  `🌟` and `⛔` mean that the check was responsible for producing an authorized or forbidden (respectively) status.

  If no check results in a status (they all have `⬇`) then the policy is assumed to have failed. In some cases, however, the policy
  may have just been ignored, as described above.
  """

  @doc """
  Print a report of an authorization failure

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
              must_pass_strict_check? =
                if must_pass_strict_check? do
                  """
                  Scenario must pass strict check only, meaning `runtime` policies cannot be checked.

                  This requirement is generally used for filtering on related resources, when we can't fetch those
                  related resources to run `runtime` policies. For this reason, you generally want your primary read
                  actions on your resources to have standard policies which can be checked statically (like `actor_attribute_equals`)
                  in addition to filter policies, like `expr(foo == :bar)`.
                  """
                else
                  ""
                end

              policy_breakdown_title =
                if Keyword.get(opts, :help_text?, true) do
                  ["Policy Breakdown", @help_text]
                else
                  "Policy Breakdown"
                end

              policy_explanation =
                policies
                |> Enum.filter(&relevant?(&1, facts))
                |> Enum.map(&explain_policy(&1, facts))
                |> Enum.intersperse("\n")
                |> title(policy_breakdown_title, false)

              filter =
                if filter do
                  title(
                    "Did not match filter expression #{inspect(filter)}",
                    "Generated Filter"
                  )
                else
                  ""
                end

              [must_pass_strict_check?, filter, policy_explanation]
              |> Enum.filter(& &1)
              |> Enum.intersperse("\n\n")
          end)
          |> Enum.intersperse("\n\n")

        [title_line(error), "\n", error_lines]
        |> IO.iodata_to_binary()
        |> String.trim()
    end
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
    Enum.all?(policy.condition, fn condition ->
      Policy.fetch_fact(facts, condition) == {:ok, true}
    end)
  end

  defp title(other, title, semicolon \\ true)
  defp title([], _, _), do: []
  defp title(other, title, true), do: [title, ":\n", other]
  defp title(other, title, false), do: [title, "\n", other]

  defp explain_policy(policy, facts) do
    bypass =
      if policy.bypass? do
        "Bypass: "
      else
        ""
      end

    {description, state} = describe_checks(policy.policies, facts)

    tag =
      case state do
        :unknown ->
          "⛔"

        :authorized ->
          "🌟"

        :forbidden ->
          "⛔"
      end

    title(Enum.map(description, &["    ", &1]), [
      "  ",
      bypass,
      policy.description || "Policy",
      " | ",
      tag
    ])
  end

  defp describe_checks(checks, facts) do
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

        tag =
          case {state, new_state} do
            {:unknown, :authorized} ->
              "🌟"

            {:unknown, :forbidden} ->
              "⛔"

            {:unknown, :unknown} ->
              "⬇"

            _ ->
              ""
          end

        {[describe_check(check, Policy.fetch_fact(facts, check.check), tag) | descriptions],
         new_state}
      end)

    {Enum.intersperse(Enum.reverse(description), "\n"), state}
  end

  defp describe_check(check, fact_result, tag) do
    fact_result =
      case fact_result do
        {:ok, true} ->
          "✓"

        {:ok, false} ->
          "✘"

        :error ->
          "?"
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

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def message(error) do
      if error.policy_breakdown? do
        "forbidden:\n\n#{Ash.Error.Forbidden.Policy.report(error, help_text?: false)}"
      else
        "forbidden"
      end
    end

    def code(_), do: "Forbidden"
  end
end
