defmodule Ash.Error.Forbidden.Policy do
  @moduledoc "Raised when policy authorization for an action fails"

  require Logger

  alias Ash.Policy.Policy

  use Splode.Error,
    fields: [
      scenarios: [],
      facts: %{},
      filter: nil,
      actor: nil,
      policy_breakdown?: false,
      must_pass_strict_check?: false,
      for_fields: nil,
      subject: nil,
      context_description: nil,
      policies: [],
      resource: nil,
      solver_statement: nil,
      domain: nil,
      action: nil,
      changeset_doesnt_match_filter: false
    ],
    class: :forbidden

  def exception(opts) do
    exception =
      super(Keyword.put(opts, :policy_breakdown?, Ash.Policy.Info.show_policy_breakdowns?()))

    log_level =
      if exception.for_fields do
        Ash.Policy.Info.log_successful_policy_breakdowns()
      else
        Ash.Policy.Info.log_policy_breakdowns()
      end

    log_level =
      log_level || (opts[:subject] && opts[:subject].context[:private][:authorizer_log?] && :info)

    if log_level do
      Logger.log(log_level, report(exception, help_text?: false))
    else
      :ok
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
  def report(%Ash.Error.Forbidden{errors: errors}) do
    errors
    |> Enum.filter(&(&1.__struct__ == __MODULE__))
    |> Enum.map_join("\n\n", &report/1)
  end

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
                  domain: error.domain,
                  resource: error.resource,
                  actor: error.actor,
                  solver_statement: error.solver_statement,
                  must_pass_strict_check?: must_pass_strict_check?,
                  subject: error.subject,
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
        ["Policy Breakdown#{policy_context_description}"]
      end

    actor =
      case opts[:actor] do
        nil ->
          "unknown actor"

        %resource{} = actor ->
          if Ash.Resource.Info.resource?(resource) do
            case Ash.Resource.Info.primary_key(actor) do
              [] ->
                "  Actor: #{inspect(actor)}"

              fields ->
                "  #{Ash.Resource.Info.short_name(resource)}: #{inspect(Map.take(actor, fields))}"
            end
          else
            "  Actor: #{inspect(actor)}"
          end

        actor ->
          "  Actor: #{inspect(actor)}"
      end

    solver_statement =
      if opts[:solver_statement] && opts[:solver_statement] not in [nil, false, true] do
        Ash.Policy.Policy.debug_expr(opts[:solver_statement], "SAT Solver statement")
      end

    policy_explanation =
      policies
      |> Kernel.||([])
      |> Enum.filter(&relevant?(&1, facts))
      |> case do
        [] ->
          # If no policies are relevant, then we treat them all as relevant
          title =
            case policies do
              [] ->
                if opts[:domain] && opts[:resource] do
                  policy_breakdown_title ++
                    [
                      "No policies defined on `#{inspect(opts[:domain])}` or `#{inspect(opts[:resource])}` that applied.\nFor safety, at least one policy must apply to all requests.\n"
                    ]
                else
                  policy_breakdown_title ++
                    [
                      "No policies defined.\n"
                    ]
                end

              _ ->
                [
                  "No policy conditions applied to this request.\nFor safety, at least one policy must apply to all requests.\n"
                ]
            end

          {[], title}

        relevant ->
          {relevant, policy_breakdown_title}
      end
      |> then(fn {policies, title} ->
        policies
        |> Enum.map(
          &explain_policy(
            &1,
            facts,
            opts[:success?] || false,
            opts[:actor],
            opts[:subject],
            opts[:resource]
          )
        )
        |> Enum.intersperse("\n")
        |> then(fn list ->
          Enum.concat([actor, "\n\n"], list)
        end)
        |> title(title, false)
      end)

    filter =
      if filter do
        title(
          "#{nicely_formatted_filter(filter)}",
          "Generated Filter"
        )
      else
        ""
      end

    [must_pass_strict_check?, filter, policy_explanation, List.wrap(solver_statement)]
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
      Policy.fetch_fact(facts, condition) != {:ok, false}
    end)
  end

  defp title(other, title, semicolon \\ true)
  defp title(other, title, true), do: [title, ":\n", other]
  defp title(other, title, false), do: [title, "\n", other]

  defp explain_policy(policy, facts, success?, actor, subject, resource) do
    bypass =
      if policy.bypass? do
        "Bypass: "
      else
        ""
      end

    {condition_description, applies} =
      describe_conditions(policy.condition, resource, facts, actor, subject)

    if applies == true do
      {description, state} =
        describe_checks(policy.policies, resource, facts, success?, actor, subject)

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
        [
          "\n",
          Enum.map(condition_description, &["    ", &1]),
          "\n",
          Enum.map(description, &["    ", &1]),
          "\n"
        ],
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

  defp describe_conditions(condition, resource, facts, actor, subject) do
    condition
    |> List.wrap()
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

      {[["condition: ", describe(mod, opts, resource, actor, subject) <> "\n"] | conditions],
       new_status}
    end)
    |> then(fn {conditions, status} ->
      {Enum.reverse(conditions), status}
    end)
  end

  defp describe_checks(checks, resource, facts, success?, actor, subject) do
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
             resource,
             Policy.fetch_fact(facts, check.check),
             tag,
             success?,
             filter_check?,
             actor,
             subject
           )
           | descriptions
         ], new_state}
      end)

    {Enum.intersperse(Enum.reverse(description), "\n"), state}
  end

  defp describe_check(check, resource, fact_result, tag, success?, filter_check?, actor, subject) do
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
      describe(check.check_module, check.check_opts, resource, actor, subject),
      " | ",
      fact_result,
      " | ",
      tag
    ]
  end

  defp describe(mod, opts, resource, actor, subject) do
    description = mod.describe(opts)

    if subject && function_exported?(mod, :expand_description, 3) do
      authorizer =
        %Ash.Policy.Authorizer{
          subject: subject,
          actor: actor,
          resource: resource
        }

      key =
        case subject do
          %Ash.Changeset{} -> :changeset
          %Ash.Query{} -> :query
          %Ash.ActionInput{} -> :action_input
        end

      authorizer = Map.put(authorizer, key, subject)

      case mod.expand_description(actor, authorizer, opts) do
        {:ok, desc} ->
          if mod.prefer_expanded_description?() do
            desc
          else
            description <> " | #{desc}"
          end

        _ ->
          description
      end
    else
      description
    end
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
