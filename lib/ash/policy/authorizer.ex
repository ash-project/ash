defmodule Ash.Policy.Authorizer do
  defstruct [
    :actor,
    :resource,
    :query,
    :changeset,
    :data,
    :action,
    :api,
    :verbose?,
    :scenarios,
    :real_scenarios,
    :check_scenarios,
    policies: [],
    facts: %{true => true, false => false},
    data_facts: %{}
  ]

  @type t :: %__MODULE__{}

  alias Ash.Policy.{Checker, Policy}

  @check_schema [
    check: [
      type: {:custom, __MODULE__, :validate_check, []},
      required: true,
      doc: """
      The check to run.
      """,
      links: [
        modules: [
          "ash:module:Ash.Policy.Check"
        ]
      ]
    ],
    name: [
      type: :string,
      required: false,
      doc:
        "A short name or description for the check, used when explaining authorization results",
      links: []
    ]
  ]

  @authorize_if %Spark.Dsl.Entity{
    name: :authorize_if,
    describe: "If the check is true, the request is authorized, otherwise run remaining checks.",
    args: [:check],
    schema: @check_schema,
    examples: [
      "authorize_if logged_in()",
      "authorize_if actor_attribute_matches_record(:group, :group)"
    ],
    links: [],
    target: Ash.Policy.Check,
    transform: {Ash.Policy.Check, :transform, []},
    auto_set_fields: [
      type: :authorize_if
    ]
  }

  @forbid_if %Spark.Dsl.Entity{
    name: :forbid_if,
    describe: "If the check is true, the request is forbidden, otherwise run remaining checks.",
    args: [:check],
    schema: @check_schema,
    target: Ash.Policy.Check,
    transform: {Ash.Policy.Check, :transform, []},
    links: [],
    examples: [
      "forbid_if not_logged_in()",
      "forbid_if actor_attribute_matches_record(:group, :blacklisted_groups)"
    ],
    auto_set_fields: [
      type: :forbid_if
    ]
  }

  @authorize_unless %Spark.Dsl.Entity{
    name: :authorize_unless,
    describe: "If the check is false, the request is authorized, otherwise run remaining checks.",
    args: [:check],
    schema: @check_schema,
    target: Ash.Policy.Check,
    transform: {Ash.Policy.Check, :transform, []},
    links: [],
    examples: [
      "authorize_unless not_logged_in()",
      "authorize_unless actor_attribute_matches_record(:group, :blacklisted_groups)"
    ],
    auto_set_fields: [
      type: :authorize_unless
    ]
  }

  @forbid_unless %Spark.Dsl.Entity{
    name: :forbid_unless,
    describe: "If the check is true, the request is forbidden, otherwise run remaining checks.",
    args: [:check],
    links: [],
    schema: @check_schema,
    target: Ash.Policy.Check,
    transform: {Ash.Policy.Check, :transform, []},
    examples: [
      "forbid_unless logged_in()",
      "forbid_unless actor_attribute_matches_record(:group, :group)"
    ],
    auto_set_fields: [
      type: :forbid_unless
    ]
  }

  @policy %Spark.Dsl.Entity{
    name: :policy,
    links: [
      guides: [
        "ash:guide:Policies"
      ]
    ],
    describe: """
    A policy has a name, a condition, and a list of checks.

    Checks apply logically in the order they are specified, from top to bottom.
    If no check explicitly authorizes the request, then the request is forbidden.
    This means that, if you want to "blacklist" instead of "whitelist", you likely
    want to add an `authorize_if always()` at the bottom of your policy, like so:

    ```elixir
    policy action_type(:read) do
      forbid_if not_logged_in()
      forbid_if user_is_denylisted()
      forbid_if user_is_in_denylisted_group()

      authorize_if always()
    end
    ```

    If the policy should always run, use the `always()` check, like so:

    ```elixir
    policy always() do
      ...
    end
    ```
    """,
    schema: [
      description: [
        type: :string,
        doc: "A description for the policy, used when explaining authorization results",
        links: []
      ],
      access_type: [
        type: {:one_of, [:strict, :filter, :runtime]},
        links: [],
        doc: """
        What portion of the checks inside the policy are allowed to run. See the guide for more.
        """
      ],
      condition: [
        type: {:custom, __MODULE__, :validate_condition, []},
        links: [],
        doc: """
        A check or list of checks that must be true in order for this policy to apply.
        """
      ]
    ],
    args: [:condition],
    target: Ash.Policy.Policy,
    entities: [
      policies: [
        @authorize_if,
        @forbid_if,
        @authorize_unless,
        @forbid_unless
      ]
    ]
  }

  @bypass %{
    @policy
    | name: :bypass,
      auto_set_fields: [bypass?: true],
      describe:
        "A policy that, if passed, will skip all following policies. If failed, authorization moves on to the next policy"
  }

  @policies %Spark.Dsl.Section{
    name: :policies,
    describe: """
    A section for declaring authorization policies.

    Each policy that applies must pass independently in order for the
    request to be authorized.
    """,
    links: [
      guides: [
        "ash:guide:Policies"
      ]
    ],
    examples: [
      """
      policies do
        # Anything you can use in a condition, you can use in a check, and vice-versa
        # This policy applies if the actor is a super_user
        # Additionally, this policy is declared as a `bypass`. That means that this check is allowed to fail without
        # failing the whole request, and that if this check *passes*, the entire request passes.
        bypass actor_attribute_equals(:super_user, true) do
          authorize_if always()
        end

        # This will likely be a common occurrence. Specifically, policies that apply to all read actions
        policy action_type(:read) do
          # unless the actor is an active user, forbid their request
          forbid_unless actor_attribute_equals(:active, true)
          # if the record is marked as public, authorize the request
          authorize_if attribute(:public, true)
          # if the actor is related to the data via that data's `owner` relationship, authorize the request
          authorize_if relates_to_actor_via(:owner)
        end
      end
      """
    ],
    entities: [
      @policy,
      @bypass
    ],
    imports: [
      Ash.Policy.Check.Builtins,
      Ash.Filter.TemplateHelpers
    ],
    schema: [
      default_access_type: [
        type: {:one_of, [:strict, :filter, :runtime]},
        links: [],
        default: :filter,
        doc: """
        The default access type of policies for this resource.
        """
      ]
    ]
  }

  @sections [@policies]

  @moduledoc """
  An authorization extension for ash resources.

  To add this extension to a resource, add it to the list of `authorizers` like so:

  ```elixir
  use Ash.Resource,
    ...,
    authorizers: [
      Ash.Policy.Authorizer
    ]
  ```

  A resource can be given a set of policies, which are enforced on each call to a resource action.

  For reads, policies can be configured to filter out data that the actor shouldn't see, as opposed to
  resulting in a forbidden error.

  See the {{link:ash:guide:Policies}} for practical examples.

  Policies are solved/managed via a boolean satisfiability solver. To read more about boolean satisfiability,
  see this page: https://en.wikipedia.org/wiki/Boolean_satisfiability_problem. At the end of
  the day, however, it is not necessary to understand exactly how Ash takes your
  authorization requirements and determines if a request is allowed. The
  important thing to understand is that Ash may or may not run any/all of your
  authorization rules as they may be deemed unnecessary. As such, authorization
  checks should have no side effects. Ideally, the checks built-in to ash should
  cover the bulk of your needs.

  <!--- ash-hq-hide-start--> <!--- -->

  ## DSL Documentation

  ### Index

  #{Spark.Dsl.Extension.doc_index(@sections)}

  ### Docs

  #{Spark.Dsl.Extension.doc(@sections)}
  <!--- ash-hq-hide-stop--> <!--- -->
  """

  require Logger

  @behaviour Ash.Authorizer

  use Spark.Dsl.Extension, sections: @sections

  @impl true
  def exception({:changeset_doesnt_match_filter, filter}, state) do
    Ash.Error.Forbidden.Policy.exception(
      scenarios: Map.get(state, :scenarios),
      facts: Map.get(state, :facts),
      policies: Map.get(state, :policies),
      resource: Map.get(state, :resource),
      action: Map.get(state, :action),
      changeset_doesnt_match_filter: true,
      filter: filter
    )
  end

  def exception(:must_pass_strict_check, state) do
    Ash.Error.Forbidden.Policy.exception(
      scenarios: Map.get(state, :scenarios),
      facts: Map.get(state, :facts),
      policies: Map.get(state, :policies),
      resource: Map.get(state, :resource),
      action: Map.get(state, :action),
      must_pass_strict_check?: true
    )
  end

  def exception(_, state) do
    Ash.Error.Forbidden.Policy.exception(
      scenarios: Map.get(state, :scenarios),
      facts: Map.get(state, :facts),
      policies: Map.get(state, :policies),
      resource: Map.get(state, :resource),
      action: Map.get(state, :action),
      must_pass_strict_check?: true
    )
  end

  @doc false
  # We can't actually validate that they are check modules here
  # without causing compile time dependencies
  def validate_check({module, opts}) when is_atom(module) and is_list(opts) do
    {:ok, {module, opts}}
  end

  def validate_check(module) when is_atom(module) do
    validate_check({module, []})
  end

  def validate_check(other) do
    {:ok, {Ash.Policy.Check.Expression, expr: other}}
  end

  def validate_condition(conditions) when is_list(conditions) do
    {:ok,
     Enum.map(conditions, fn condition ->
       {:ok, v} = condition |> validate_check()
       v
     end)}
  end

  @doc false
  def validate_condition(condition) do
    validate_condition([condition])
  end

  @impl true
  def initial_state(actor, resource, action, verbose?) do
    %__MODULE__{
      resource: resource,
      actor: actor,
      action: action,
      verbose?: verbose?
    }
  end

  @impl true
  def strict_check_context(_authorizer) do
    [:query, :changeset, :api, :resource]
  end

  @impl true
  def check_context(_authorizer) do
    [:query, :changeset, :data, :api, :resource]
  end

  @impl true
  def check(authorizer, context) do
    check_result(%{authorizer | data: context.data})
  end

  @impl true
  def strict_check(authorizer, context) do
    %{
      authorizer
      | query: context.query,
        changeset: context.changeset,
        api: context.api
    }
    |> get_policies()
    |> do_strict_check_facts()
    |> case do
      {:ok, authorizer} ->
        case strict_check_result(authorizer) do
          :authorized ->
            log_successful_policy_breakdown(authorizer)
            :authorized

          {:filter, authorizer, filter} ->
            log_successful_policy_breakdown(authorizer, filter)
            {:filter, authorizer, filter}

          other ->
            other
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp strict_filter(authorizer) do
    {filterable, require_check} =
      authorizer.scenarios
      |> Enum.split_with(fn scenario ->
        scenario
        |> Enum.reject(fn {{check_module, opts}, _} ->
          opts[:access_type] == :filter ||
            match?(
              {:ok, _},
              Ash.Policy.Policy.fetch_fact(authorizer.facts, {check_module, opts})
            ) || check_module.type() == :filter
        end)
        |> Enum.empty?()
      end)

    filter = strict_filters(filterable, authorizer)

    case {filter, require_check} do
      {[], []} ->
        :authorized

      {_filters, []} ->
        if Enum.any?(filter, &(&1 == true)) do
          :authorized
        else
          case filter do
            [filter] ->
              log(authorizer, "filtering with: #{inspect(filter)}, authorization complete")
              {:filter, authorizer, filter}

            filters ->
              log(authorizer, "filtering with: #{inspect(or: filter)}, authorization complete")
              {:filter, authorizer, [or: filters]}
          end
        end

      {_filters, _require_check} ->
        case global_filters(authorizer) do
          nil ->
            maybe_forbid_strict(authorizer)

          {[single_filter], scenarios_without_global} ->
            log(
              authorizer,
              "filtering with: #{inspect(single_filter)}, continuing authorization process"
            )

            {:filter_and_continue, single_filter,
             %{authorizer | check_scenarios: scenarios_without_global}}

          {filters, scenarios_without_global} ->
            log(
              authorizer,
              "filtering with: #{inspect(and: filters)}, continuing authorization process"
            )

            {:filter_and_continue, [and: filters],
             %{authorizer | check_scenarios: scenarios_without_global}}
        end
    end
  end

  defp strict_filters(filterable, authorizer) do
    filterable
    |> Enum.map(fn scenario ->
      scenario
      |> Enum.filter(fn {{check_module, check_opts}, _} ->
        check_module.type() == :filter && check_opts[:access_type] in [:filter, :runtime]
      end)
      |> Enum.reject(fn {{check_module, check_opts}, result} ->
        match?({:ok, ^result}, Policy.fetch_fact(authorizer.facts, {check_module, check_opts}))
      end)
      |> Map.new()
    end)
    |> Ash.Policy.SatSolver.simplify_clauses()
    |> Enum.reduce([], fn scenario, or_filters ->
      scenario
      |> Enum.map(fn
        {{check_module, check_opts}, true} ->
          result =
            try do
              check_module.auto_filter(authorizer.actor, authorizer, check_opts)
            rescue
              e ->
                reraise Ash.Error.to_ash_error(e, __STACKTRACE__,
                          error_context:
                            "Creating filter for check: #{check_module.describe(check_opts)} on resource: #{authorizer.resource}"
                        ),
                        __STACKTRACE__
            end

          if is_nil(result) do
            false
          else
            result
          end

        {{check_module, check_opts}, false} ->
          result =
            try do
              if :erlang.function_exported(check_module, :auto_filter_not, 3) do
                check_module.auto_filter_not(authorizer.actor, authorizer, check_opts)
              else
                [not: check_module.auto_filter(authorizer.actor, authorizer, check_opts)]
              end
            rescue
              e ->
                reraise Ash.Error.to_ash_error(e, __STACKTRACE__,
                          error_context:
                            "Creating filter for check: #{check_module.describe(check_opts)} on resource: #{authorizer.resource}"
                        ),
                        __STACKTRACE__
            end

          if is_nil(result) do
            false
          else
            result
          end
      end)
      |> case do
        [] ->
          or_filters

        [single] ->
          [single | or_filters]

        filters ->
          [[and: filters] | or_filters]
      end
    end)
  end

  def print_tuple_boolean({op, l, r}) when op in [:and, :or] do
    "(#{print_tuple_boolean(l)} #{op} #{print_tuple_boolean(r)})"
  end

  def print_tuple_boolean({:not, l}) do
    "not #{print_tuple_boolean(l)}"
  end

  def print_tuple_boolean({check, opts}) do
    check.describe(opts)
  end

  def print_tuple_boolean(v) do
    inspect(v)
  end

  defp maybe_forbid_strict(authorizer) do
    log(authorizer, "could not determine authorization filter, checking at runtime")
    {:continue, %{authorizer | check_scenarios: authorizer.scenarios}}
  end

  defp global_filters(authorizer, scenarios \\ nil, filter \\ []) do
    scenarios = scenarios || authorizer.scenarios

    global_check_value =
      Enum.find_value(scenarios, fn scenario ->
        Enum.find(scenario, fn {{check_module, _opts} = check, value} ->
          check_module.type == :filter &&
            Enum.all?(scenarios, &(Map.fetch(&1, check) == {:ok, value}))
        end)
      end)

    case global_check_value do
      nil ->
        case filter do
          [] ->
            nil

          filter ->
            {filter, scenarios}
        end

      {{check_module, check_opts}, required_status} ->
        additional_filter =
          if required_status do
            check_module.auto_filter(authorizer.actor, authorizer, check_opts)
          else
            if :erlang.function_exported(check_module, :auto_filter_not, 3) do
              check_module.auto_filter_not(authorizer.actor, authorizer, check_opts)
            else
              [not: check_module.auto_filter(authorizer.actor, authorizer, check_opts)]
            end
          end

        scenarios = remove_clause(authorizer.scenarios, {check_module, check_opts})
        new_facts = Map.put(authorizer.facts, {check_module, check_opts}, required_status)

        global_filters(%{authorizer | facts: new_facts}, scenarios, [additional_filter | filter])
    end
  end

  defp remove_clause(scenarios, clause) do
    Enum.map(scenarios, &Map.delete(&1, clause))
  end

  defp check_result(authorizer) do
    Enum.reduce_while(authorizer.data, {:ok, authorizer}, fn record, {:ok, authorizer} ->
      authorizer.scenarios
      |> Enum.reject(&scenario_impossible?(&1, authorizer, record))
      |> case do
        [] ->
          {:halt, {:error, :forbidden, authorizer}}

        scenarios ->
          do_check_result(scenarios, authorizer, record)
      end
    end)
    |> case do
      {:ok, authorizer} ->
        log_successful_policy_breakdown(authorizer)

      other ->
        other
    end
  end

  defp log_successful_policy_breakdown(authorizer, filter \\ nil) do
    case Ash.Policy.Info.log_successful_policy_breakdowns() do
      nil ->
        :ok

      level ->
        do_log_successful_policy_breakdown(authorizer, filter, level)
    end
  end

  defp do_log_successful_policy_breakdown(authorizer, filter, level) do
    title =
      "Successful authorization: #{inspect(authorizer.resource)}.#{authorizer.action.name}\n"

    Logger.log(
      level,
      [
        title
        | Ash.Error.Forbidden.Policy.get_breakdown(
            authorizer.facts,
            filter,
            authorizer.policies,
            success?: true
          )
      ]
    )
  end

  defp do_check_result(cleaned_scenarios, authorizer, record) do
    if Enum.any?(cleaned_scenarios, &scenario_applies?(&1, authorizer, record)) do
      {:cont, {:ok, authorizer}}
    else
      check_facts_until_known(cleaned_scenarios, authorizer, record)
    end
  end

  defp scenario_applies?(scenario, authorizer, record) do
    Enum.all?(scenario, fn {clause, requirement} ->
      case Map.fetch(authorizer.facts, clause) do
        {:ok, ^requirement} ->
          true

        _ ->
          scenario_applies_to_record?(authorizer, clause, record)
      end
    end)
  end

  defp scenario_applies_to_record?(authorizer, clause, record) do
    case Map.fetch(authorizer.data_facts, clause) do
      {:ok, ids_that_match} ->
        pkey = Map.take(record, Ash.Resource.Info.primary_key(authorizer.resource))

        MapSet.member?(ids_that_match, pkey)

      _ ->
        false
    end
  end

  defp scenario_impossible?(scenario, authorizer, record) do
    Enum.any?(scenario, fn {clause, requirement} ->
      case Map.fetch(authorizer.facts, clause) do
        {:ok, value} when value != requirement ->
          true

        _ ->
          scenario_impossible_by_data?(authorizer, clause, record)
      end
    end)
  end

  defp scenario_impossible_by_data?(authorizer, clause, record) do
    case Map.fetch(authorizer.data_facts, clause) do
      {:ok, ids_that_match} ->
        pkey = Map.take(record, Ash.Resource.Info.primary_key(authorizer.resource))

        not MapSet.member?(ids_that_match, pkey)

      _ ->
        false
    end
  end

  defp check_facts_until_known(scenarios, authorizer, record) do
    new_authorizer =
      scenarios
      |> find_fact_to_check(authorizer)
      |> check_fact(authorizer)

    scenarios
    |> Enum.reject(&scenario_impossible?(&1, new_authorizer, record))
    |> case do
      [] ->
        log(authorizer, "Checked all facts, no real scenarios")
        {:halt, {:forbidden, authorizer}}

      scenarios ->
        if Enum.any?(scenarios, &scenario_applies?(&1, new_authorizer, record)) do
          {:cont, {:ok, new_authorizer}}
        else
          check_facts_until_known(scenarios, new_authorizer, record)
        end
    end
  end

  defp check_fact({check_module, check_opts}, authorizer) do
    if check_module.type() == :simple do
      raise "Assumption failed"
    else
      if authorizer.action.type == :read ||
           Ash.DataLayer.data_layer_can?(authorizer.resource, :transact) do
        authorized_records =
          check_module.check(authorizer.actor, authorizer.data, authorizer, check_opts)

        pkey = Ash.Resource.Info.primary_key(authorizer.resource)

        pkeys = MapSet.new(authorized_records, &Map.take(&1, pkey))

        %{
          authorizer
          | data_facts: Map.put(authorizer.data_facts, {check_module, check_opts}, pkeys)
        }
      else
        raise """
        Attempted to use a `check/4` function on a non-read action with a resource who's data layer does
        not support transactions. This means that you have a policy set to `access_type :runtime` that is
        unsafe to be set as such. Authorization over create/update/destroy actions for resources that don't
        support transactions must only be done with filter and/or strict checks.
        """
      end
    end
  end

  defp find_fact_to_check(scenarios, authorizer) do
    scenarios
    |> Enum.concat()
    |> Enum.find(fn {key, _value} ->
      not Map.has_key?(authorizer.facts, key) and not Map.has_key?(authorizer.data_facts, key)
    end)
    |> case do
      nil -> raise "Assumption failed"
      {key, _value} -> key
    end
  end

  defp strict_check_result(authorizer) do
    case Checker.strict_check_scenarios(authorizer) do
      {:ok, scenarios} ->
        report_scenarios(authorizer, scenarios, "Potential Scenarios")

        case Checker.find_real_scenarios(scenarios, authorizer.facts) do
          [] ->
            maybe_strict_filter(authorizer, scenarios)

          real_scenarios ->
            report_scenarios(authorizer, real_scenarios, "Real Scenarios")
            :authorized
        end

      {:error, :unsatisfiable} ->
        {:error,
         Ash.Error.Forbidden.Policy.exception(
           facts: authorizer.facts,
           policies: authorizer.policies,
           resource: Map.get(authorizer, :resource),
           action: Map.get(authorizer, :action),
           scenarios: []
         )}
    end
  end

  defp maybe_strict_filter(authorizer, scenarios) do
    log(authorizer, "No real scenarios, attempting to filter")
    strict_filter(%{authorizer | scenarios: scenarios})
  end

  defp do_strict_check_facts(authorizer) do
    case Checker.strict_check_facts(authorizer) do
      {:ok, authorizer, new_facts} ->
        {:ok, %{authorizer | facts: new_facts}}

      {:error, error} ->
        {:error, error}
    end
  end

  defp get_policies(authorizer) do
    %{
      authorizer
      | policies: Ash.Policy.Info.policies(authorizer.resource)
    }
  end

  defp report_scenarios(%{verbose?: true}, scenarios, title) do
    scenario_description =
      scenarios
      |> Enum.map(fn scenario ->
        scenario
        |> Enum.reject(fn {{module, _}, _} ->
          module == Ash.Policy.Check.Static
        end)
        |> Enum.map(fn {{module, opts}, requirement} ->
          ["  ", module.describe(opts) <> " => #{requirement}"]
        end)
        |> Enum.intersperse("\n")
      end)
      |> Enum.intersperse("\n--\n")

    Logger.info([title, "\n", scenario_description])
  end

  defp report_scenarios(_, _, _), do: :ok

  defp log(%{verbose?: true}, message) do
    Logger.info(message)
  end

  defp log(_, _), do: :ok
end
