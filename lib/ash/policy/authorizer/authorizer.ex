defmodule Ash.Policy.Authorizer do
  defstruct [
    :actor,
    :resource,
    :query,
    :changeset,
    :action_input,
    :data,
    :action,
    :domain,
    :scenarios,
    :real_scenarios,
    :check_scenarios,
    :subject,
    :for_fields,
    :solver_statement,
    context: %{},
    policies: [],
    facts: %{true => true, false => false},
    data_facts: %{}
  ]

  @type t :: %__MODULE__{
          actor: term,
          resource: Ash.Resource.t(),
          query: Ash.Query.t() | nil,
          changeset: Ash.Changeset.t() | nil,
          action_input: Ash.ActionInput.t() | nil,
          subject: Ash.Query.t() | Ash.Changeset.t() | Ash.ActionInput.t(),
          context: map,
          data: term,
          solver_statement: term,
          action: Ash.Resource.Actions.Action.t(),
          domain: Ash.Domain.t(),
          scenarios: [map],
          real_scenarios: [map],
          check_scenarios: [map],
          policies: [term],
          facts: map(),
          data_facts: map()
        }

  require Ash.Expr
  require Ash.Sort

  alias Ash.Policy.{Checker, Policy}

  @check_schema [
    check: [
      type:
        {:or,
         [
           {:custom, __MODULE__, :template_var, []},
           {:spark_behaviour, Ash.Policy.Check, Ash.Policy.Check.Builtins},
           {:custom, __MODULE__, :expr_check, []}
         ]},
      required: true,
      doc: """
      The check to run. See `Ash.Policy.Check` for more.
      """
    ],
    name: [
      type: :string,
      required: false,
      doc: "A short name or description for the check, used when explaining authorization results"
    ]
  ]

  @authorize_if %Spark.Dsl.Entity{
    name: :authorize_if,
    describe: "If the check is true, the request is authorized, otherwise run remaining checks.",
    args: [:check],
    schema: @check_schema,
    no_depend_modules: [:check],
    examples: [
      "authorize_if logged_in()",
      "authorize_if actor_attribute_matches_record(:group, :group)"
    ],
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
    no_depend_modules: [:check],
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
    describe: "If the check is true, run remaining checks, otherwise the request is authorized.",
    args: [:check],
    schema: @check_schema,
    target: Ash.Policy.Check,
    transform: {Ash.Policy.Check, :transform, []},
    no_depend_modules: [:check],
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
    describe: "If the check is true, run remaining checks, otherwise the request is forbidden.",
    args: [:check],
    schema: @check_schema,
    target: Ash.Policy.Check,
    no_depend_modules: [:check],
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

    See the [policies guide](/documentation/topics/security/policies.md) for more.
    """,
    schema: [
      description: [
        type: :string,
        doc: "A description for the policy, used when explaining authorization results"
      ],
      access_type: [
        type: {:one_of, [:strict, :filter, :runtime]},
        doc: """
        Determines how the policy is applied. See the guide for more.
        """
      ],
      condition: [
        type: {:custom, __MODULE__, :validate_condition, []},
        doc: """
        A check or list of checks that must be true in order for this policy to apply.
        """
      ]
    ],
    args: [{:optional, :condition}],
    target: Ash.Policy.Policy,
    no_depend_modules: [:condition],
    transform: {Ash.Policy.Policy, :transform, []},
    entities: [
      policies: [
        @authorize_if,
        @forbid_if,
        @authorize_unless,
        @forbid_unless
      ]
    ]
  }

  @policy_group %Spark.Dsl.Entity{
    name: :policy_group,
    target: Ash.Policy.PolicyGroup,
    transform: {Ash.Policy.PolicyGroup, :transform, []},
    describe: """
    Groups a set of policies together by some condition.

    If the condition on the policy group does not apply, then none of the policies within it apply.

    This is primarily syntactic sugar. At compile time, the conditions from the policy group are
    added to each policy it contains, and the list is flattened out. This exists primarily to make it
    easier to reason about and write policies.

    The following are equivalent:

    ```elixir
    policy_group condition1 do
      policy condition2 do
        ...
      end

      policy condition3 do
        ...
      end
    end
    ```

    and

    ```elixir
    policy [condition1, condition2] do
      ...
    end

    policy [condition1, condition3] do
      ...
    end
    ```
    """,
    schema: [
      condition: [
        type: {:custom, __MODULE__, :validate_condition, []},
        doc: """
        A check or list of checks that must be true in order for this policy to apply.
        """
      ]
    ],
    args: [:condition],
    no_depend_modules: [:condition],
    recursive_as: :policies,
    entities: [
      policies: [
        @policy
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

    See the [policies guide](/documentation/topics/security/policies.md) for more.
    """,
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
      @policy_group,
      @bypass
    ],
    imports: [
      Ash.Policy.Check.Builtins,
      Ash.Expr
    ],
    schema: [
      default_access_type: [
        type: {:one_of, [:strict, :filter, :runtime]},
        default: :filter,
        doc: """
        The default access type of policies for this resource.
        """
      ]
    ]
  }

  @field_policy %Spark.Dsl.Entity{
    name: :field_policy,
    describe: """
    Field policies behave similarly to policies. See `d:Ash.Policy.Authorizer.field_policies`
    for more.
    """,
    identifier: {:auto, :unique_integer},
    schema: [
      description: [
        type: :string,
        doc: "A description for the policy, used when explaining authorization results"
      ],
      fields: [
        type: {:wrap_list, :atom},
        doc: "The field or fields that the policy applies to."
      ],
      condition: [
        type: {:custom, __MODULE__, :validate_condition, []},
        doc:
          "A check or list of checks that must be true in order for this field policy to apply. If not specified, it always applies."
      ]
    ],
    args: [:fields, {:optional, :condition}],
    target: Ash.Policy.FieldPolicy,
    transform: {Ash.Policy.FieldPolicy, :transform, []},
    entities: [
      policies: [
        @authorize_if,
        @forbid_if,
        @authorize_unless,
        @forbid_unless
      ]
    ]
  }

  @field_policy_bypass %{
    @field_policy
    | name: :field_policy_bypass,
      auto_set_fields: [bypass?: true],
      describe:
        "A field policy that, if passed, will skip all following field policies for that field or fields. If failed, field authorization moves on to the next policy"
  }

  @field_policies %Spark.Dsl.Section{
    name: :field_policies,
    imports: [
      Ash.Policy.Check.Builtins,
      Ash.Expr
    ],
    describe: """
    Authorize access to specific fields via policies scoped to fields.

    If *any* field policies exist then *all* fields must be authorized by a field policy.
    If you want a "deny-list" style, then you can add policies for specific fields
    and add a catch-all policy using the special field name `:*`. All policies that apply
    to a field must be authorized.

    The only exception to the above behavior is primary keys, which can always be read by everyone.

    Additionally, keep in mind that adding `Ash.Policy.Authorizer` will require that all actions
    pass policies. If you want to just add field policies, you will need to add a policy that allows
    all access explicitly, i.e

    ```elixir
    policies do
      policy always() do
        authorize_if always()
      end
    end
    ```

    Using expressions: unlike in regular policies, expressions in field policies cannot refer
    to related entities currently. Instead, you will need to create aggregates or expression calculations
    that return the results you want to reference.

    In results, forbidden fields will be replaced with a special value: `%Ash.ForbiddenField{}`.

    When these fields are referred to in filters, they will be replaced with an expression that evaluates
    to `nil`. To support this behavior, only expression/filter checks are allowed in field policies.
    """,
    examples: [
      """
      field_policies do
        field_policy :admin_only_field do
          authorize_if actor_attribute_equals(:admin, true)
        end
      end
      """,
      """
      # Example of denylist style
      field_policies do
        field_policy [:sensitive, :fields] do
          authorize_if actor_attribute_equals(:admin, true)
        end

        field_policy :* do
          authorize_if always()
        end
      end
      """
    ],
    entities: [
      @field_policy_bypass,
      @field_policy
    ],
    schema: [
      private_fields: [
        type: {:one_of, [:show, :hide, :include]},
        default: Application.compile_env(:ash, :policies)[:private_fields] || :show,
        doc: """
        How private fields should be handled by field policies in internal functions. See the [Policies guide](documentation/topics/security/policies.md#field-policies) for more.
        """
      ]
    ]
  }

  @sections [@policies, @field_policies]

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

  See the [policies guide](/documentation/topics/security/policies.md) for practical examples.

  Policies are solved/managed via a boolean satisfiability solver. To read more about boolean satisfiability,
  see this page: https://en.wikipedia.org/wiki/Boolean_satisfiability_problem. At the end of
  the day, however, it is not necessary to understand exactly how Ash takes your
  authorization requirements and determines if a request is allowed. The
  important thing to understand is that Ash may or may not run any/all of your
  authorization rules as they may be deemed unnecessary. As such, authorization
  checks should have no side effects. Ideally, the checks built-in to ash should
  cover the bulk of your needs.
  """

  require Logger

  use Ash.Authorizer

  @transformers [
    Ash.Policy.Authorizer.Transformers.AddMissingFieldPolicies,
    Ash.Policy.Authorizer.Transformers.CacheFieldPolicies
  ]

  @verifiers [
    Ash.Policy.Authorizer.Verifiers.VerifyInAuthorizers,
    Ash.Policy.Authorizer.Verifiers.VerifySatSolverImplementation,
    Ash.Policy.Authorizer.Verifiers.VerifyResources
  ]

  use Spark.Dsl.Extension,
    sections: @sections,
    transformers: @transformers,
    verifiers: @verifiers

  @impl true
  def exception({:changeset_doesnt_match_filter, filter}, state) do
    Ash.Error.Forbidden.Policy.exception(
      scenarios: Map.get(state, :scenarios),
      solver_statement: Map.get(state, :solver_statement),
      facts: Map.get(state, :facts),
      policies: Map.get(state, :policies),
      subject: Map.get(state, :subject),
      resource: Map.get(state, :resource),
      action: Map.get(state, :action),
      actor: Map.get(state, :actor),
      domain: Map.get(state, :domain),
      changeset_doesnt_match_filter: true,
      filter: filter
    )
  end

  def exception(:must_pass_strict_check, state) do
    Ash.Error.Forbidden.Policy.exception(
      scenarios: Map.get(state, :scenarios),
      facts: Map.get(state, :facts),
      solver_statement: Map.get(state, :solver_statement),
      domain: Map.get(state, :domain),
      subject: Map.get(state, :subject),
      policies: Map.get(state, :policies),
      resource: Map.get(state, :resource),
      action: Map.get(state, :action),
      actor: Map.get(state, :actor),
      must_pass_strict_check?: true
    )
  end

  def exception(_, state) do
    Ash.Error.Forbidden.Policy.exception(
      scenarios: Map.get(state, :scenarios),
      domain: Map.get(state, :domain),
      solver_statement: Map.get(state, :solver_statement),
      facts: Map.get(state, :facts),
      subject: Map.get(state, :subject),
      policies: Map.get(state, :policies),
      resource: Map.get(state, :resource),
      action: Map.get(state, :action),
      actor: Map.get(state, :actor),
      must_pass_strict_check?: false
    )
  end

  if Code.ensure_loaded?(Igniter) do
    def install(igniter, module, type, _path, argv) do
      yes = "--yes" in argv or "-y" in argv

      igniter =
        with {:ok, nil} <-
               Igniter.Project.Deps.get_dep(igniter, :picosat_elixir),
             {:ok, nil} <- Igniter.Project.Deps.get_dep(igniter, :simple_sat) do
          {solver, notice_type} =
            if yes do
              case :os.type() do
                {_, :nt} ->
                  {{:simple_sat, "~> 0.1"}, :windows}

                _ ->
                  {{:picosat_elixir, "~> 0.2"}, :not_windows}
              end
            else
              case :os.type() do
                {_, :nt} ->
                  {Igniter.Util.IO.select(
                     """
                     Ash.Policy.Authorizer requires a SAT solver (Boolean Satisfiability Solver). This solver is used to
                     check policy requirements to answer questions like "Is this user allowed to do this action?" and
                     "What filter must be applied to this query to show only the allowed records a user can see?".

                     Which SAT solver would you like to use?

                     1. `:picosat_elixir` (recommended) - A NIF wrapper around the PicoSAT SAT solver. Fast, production ready, battle tested.
                     2. `:simple_sat` (only if necessary) - A pure Elixir SAT solver. Slower than PicoSAT, but no NIF dependency.
                     """,
                     [
                       {:picosat_elixir, "~> 0.2"},
                       {:simple_sat, "~> 0.1"}
                     ],
                     display: &to_string(elem(&1, 0))
                   ), nil}

                _ ->
                  {{:picosat_elixir, "~> 0.2"}, :not_windows}
              end
            end

          igniter =
            case notice_type do
              :windows ->
                notice =
                  """
                  Ash.Policy.Authorizer requires a SAT solver (Boolean Satisfiability Solver). This solver is used to
                  check policy requirements to answer questions like "Is this user allowed to do this action?" and
                  "What filter must be applied to this query to show only the allowed records a user can see?".

                  We detected that you are using windows, and so have installed `:simple_sat`, due to
                  users on windows often having trouble installing `:picosat_elixir`.

                  You can stick with `:simple_sat`, but we suggest switching to `:picosat_elixir` at some point,
                  for performance reasons.

                  For more, see the documentation for `:picosat_elixir`: https://github.com/bitwalker/picosat_elixir
                  """

                Igniter.add_notice(igniter, notice)

              :not_windows ->
                notice =
                  """
                  Ash.Policy.Authorizer requires a SAT solver (Boolean Satisfiability Solver). This solver is used to
                  check policy requirements to answer questions like "Is this user allowed to do this action?" and
                  "What filter must be applied to this query to show only the allowed records a user can see?".

                  We have installed `:picosat_elixir` by default. This can occasionally cause problems for some users.

                  If you encounter issues compiling `:picosat_elixir`, first remove it from your dependencies list,
                  and then run:

                      mix igniter.install simple_sat && mix deps.compile ash --force
                  """

                Igniter.add_notice(igniter, notice)

              _ ->
                igniter
            end

          igniter
          |> Igniter.Project.Deps.add_dep(solver, yes?: yes)
          |> Igniter.apply_and_fetch_dependencies(yes: yes, yes_to_deps: true)
        else
          _ ->
            igniter
        end

      igniter
      |> Spark.Igniter.add_extension(
        module,
        type,
        :authorizers,
        Ash.Policy.Authorizer
      )
    end
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
  def initial_state(actor, resource, action, domain) do
    %__MODULE__{
      resource: resource,
      actor: actor,
      action: action,
      domain: domain,
      policies: Ash.Policy.Info.policies(domain, resource)
    }
  end

  @impl true
  def strict_check_context(_authorizer) do
    [:query, :changeset, :domain, :resource, :action_input]
  end

  @impl true
  def check_context(_authorizer) do
    [:query, :changeset, :data, :domain, :resource, :action_input]
  end

  @impl true
  def check(authorizer, context) do
    check_result(%{authorizer | data: context.data})
  end

  @impl true
  def strict_check(authorizer, context) do
    authorizer
    |> ensure_context_in_authorizer(context)
    |> get_policies()
    |> strict_check_result()
    |> case do
      {:authorized, authorizer} ->
        log_successful_policy_breakdown(authorizer)
        {:authorized, authorizer}

      {:filter, authorizer, filter} ->
        log_successful_policy_breakdown(authorizer, filter)
        {:filter, authorizer, filter}

      {:filter_and_continue, filter, authorizer} ->
        log_successful_policy_breakdown(authorizer, filter)
        {:filter, authorizer, filter}

      {:error, error} ->
        {:error, error}

      {:continue, authorizer} ->
        {:continue, authorizer}

      {other, _authorizer} ->
        other
    end
  end

  @impl true
  def alter_filter(
        %Ash.Filter{expression: expression, resource: resource} = filter,
        authorizer,
        context
      ) do
    authorizer = ensure_context_in_authorizer(authorizer, context)

    {expr, _acc} =
      replace_refs(expression, authorizer_acc(authorizer, resource, context))

    {:ok, %{filter | expression: expr}}
  end

  def alter_filter(filter, _, _), do: {:ok, filter}

  defp authorizer_acc(authorizer, resource, context) do
    %{
      stack: [{resource, [], context.query.action, context.query.domain}],
      authorizers: %{
        {resource, context.query.action} => authorizer
      },
      actor: authorizer.actor
    }
  end

  def alter_sort(
        sort,
        authorizer,
        context
      ) do
    authorizer = ensure_context_in_authorizer(authorizer, context)

    case Ash.Policy.Info.field_policies(authorizer.resource) do
      [] ->
        {:ok, sort}

      _ ->
        altered =
          sort
          |> Enum.with_index()
          |> Enum.reduce(
            {[], authorizer_acc(authorizer, authorizer.resource, context)},
            fn {sort, index}, {new_sorts, acc} ->
              {sort, acc} =
                if index in context.query.sort_input_indices do
                  authorize_sort(sort, authorizer, context, acc)
                else
                  {sort, acc}
                end

              {new_sorts ++ [sort], acc}
            end
          )
          |> elem(0)

        {:ok, altered}
    end
  end

  defp authorize_sort({field, data}, authorizer, context, acc) do
    field_name =
      case field do
        %Ash.Query.Calculation{} = calculation ->
          calculation.calc_name

        field when is_atom(field) ->
          field
      end

    type = get_type(authorizer.resource, field)

    {path, field, actual_field, action, domain} =
      case {field_name, field} do
        {nil,
         %Ash.Query.Calculation{module: Ash.Resource.Calculation.Expression, opts: opts} = calc} ->
          field_and_path =
            case opts[:expr] do
              %Ash.Query.Function.Type{arguments: [%Ash.Query.Ref{} = ref | _]} ->
                {ref.relationship_path, ref.attribute.name}

              %Ash.Query.Function.Type{arguments: [{:_ref, path, field} | _]} ->
                {path, field}

              %Ash.Query.Call{name: :type, args: [%Ash.Query.Ref{} = ref | _]} ->
                {ref.relationship_path, ref.attribute}

              %Ash.Query.Call{name: :type, args: [{:_ref, path, field} | _]} ->
                {path, field}

              %Ash.Query.Ref{} = ref ->
                {ref.relationship_path, ref.attribute}

              {:_ref, path, field} ->
                {path, field}

              _ ->
                nil
            end

          case field_and_path do
            {path, %Ash.Query.Calculation{calc_name: calc_name}} when not is_nil(calc_name) ->
              {path, calc_name}

            {path, %Ash.Query.Aggregate{agg_name: agg_name}} when not is_nil(agg_name) ->
              {path, agg_name}

            {_, %Ash.Query.Calculation{}} ->
              nil

            {_, %Ash.Query.Aggregate{}} ->
              nil

            {path, %{name: name}} when not is_nil(name) ->
              {path, name}

            {path, field} when is_atom(field) and not is_nil(field) ->
              {path, field}

            _ ->
              nil
          end
          |> then(fn
            nil ->
              raise Ash.Error.Framework.AssumptionFailed,
                message: """
                It should not be possible to provide a non-resource calculation as user input.
                In the future it will be, and that will need to be addressed here.
                This error message is to prevent forgetting to address that reality.

                Got:

                  #{inspect(calc)}
                """

            {path, field} ->
              relationship = Ash.Resource.Info.relationship(authorizer.resource, path)

              domain =
                Ash.Domain.Info.related_domain(
                  relationship.destination,
                  relationship,
                  relationship.domain || authorizer.domain
                )

              action =
                case relationship.read_action do
                  nil ->
                    Ash.Resource.Info.primary_action!(relationship.destination, :read)

                  read_action ->
                    Ash.Resource.Info.action(relationship.destination, read_action)
                end

              {path, field, calc, action, domain}
          end)

        {nil, %Ash.Query.Calculation{} = calculation} ->
          raise Ash.Error.Framework.AssumptionFailed,
            message: """
            It should not be possible to provide a non-resource calculation as user input.
            In the future it will be, and that will need to be addressed here.
            This error message is to prevent forgetting to address that reality.

            Got:

              #{inspect(calculation)}
            """

        {_other, field} ->
          {[], field, field, context.query.action, context.query.domain}
      end

    resource =
      Ash.Resource.Info.related(authorizer.resource, path)

    if field_name do
      case field_condition(
             resource,
             field,
             action,
             domain,
             acc
           ) do
        {:none, acc} ->
          {{actual_field, data}, acc}

        {:expr, expr, acc} ->
          field =
            case actual_field do
              %Ash.Query.Calculation{} = calculation ->
                %Ash.Query.Ref{
                  attribute: calculation,
                  relationship_path: [],
                  resource: context.query.resource,
                  input?: false
                }

              field when is_atom(field) ->
                %Ash.Query.Ref{
                  attribute: Ash.Resource.Info.field(context.query.resource, field),
                  relationship_path: [],
                  resource: context.query.resource,
                  input?: false
                }
            end

          {type, constraints} =
            case type do
              {:array, _} -> {type, []}
              {type, constraints} -> {type, constraints}
              type -> {type, nil}
            end

          expr =
            Ash.Expr.calc(
              if ^expr do
                ^field
              else
                nil
              end,
              type: type,
              constraints: constraints
            )

          {{expr, data}, acc}
      end
    else
      {{actual_field, data}, acc}
    end
  end

  defp get_type(_resource, %{type: type, constraints: constraints}), do: {type, constraints}

  defp get_type(resource, field) do
    case Ash.Resource.Info.field(resource, field) do
      %Ash.Resource.Aggregate{kind: kind, field: field, relationship_path: relationship_path} ->
        if field do
          related = Ash.Resource.Info.related(resource, relationship_path)
          {field_type, constraints} = get_type(related, field)
          Ash.Query.Aggregate.kind_to_type(kind, field_type, constraints)
        else
          Ash.Query.Aggregate.kind_to_type(kind, nil, nil)
        end

      %{type: type, constraints: constraints} ->
        {type, constraints}
    end
  end

  defp replace_refs(expression, acc) do
    case expression do
      %Ash.Query.BooleanExpression{op: op, left: left, right: right} ->
        {left, acc} = replace_refs(left, acc)
        {right, acc} = replace_refs(right, acc)
        {Ash.Query.BooleanExpression.optimized_new(op, left, right), acc}

      %Ash.Query.Not{expression: not_expr} = expr ->
        {not_expr, acc} = replace_refs(not_expr, acc)
        {%{expr | expression: not_expr}, acc}

      %Ash.Query.Parent{expr: expr} = this ->
        original_stack = acc.stack
        {expr, acc} = replace_refs(expr, %{acc | stack: Enum.drop(acc.stack, 1)})

        {%{this | expr: expr}, %{acc | stack: original_stack}}

      %Ash.CustomExpression{expression: expression, simple_expression: simple_expression} =
          custom_expression ->
        {expression, acc} = replace_refs(expression, acc)
        {simple_expression, acc} = replace_refs(simple_expression, acc)
        {%{custom_expression | expression: expression, simple_expression: simple_expression}, acc}

      %Ash.Query.Exists{expr: expr, at_path: at_path, path: path} = exists ->
        full_path = at_path ++ path
        [{resource, current_path, _, domain} | _] = acc.stack
        {resource, action} = related_with_action(resource, full_path)

        {expr, acc} =
          replace_refs(expr, %{
            acc
            | stack: [{resource, current_path ++ full_path, action, domain} | acc.stack]
          })

        {%{exists | expr: expr}, %{acc | stack: tl(acc.stack)}}

      %{__operator__?: true, left: left, right: right} = op ->
        {left, acc} = replace_refs(left, acc)
        {right, acc} = replace_refs(right, acc)

        {%{op | left: left, right: right}, acc}

      %{__function__?: true, arguments: arguments} = function ->
        {args, acc} =
          Enum.reduce(arguments, {[], acc}, fn arg, {args, acc} ->
            {arg, acc} = replace_refs(arg, acc)
            {[arg | args], acc}
          end)

        {%{function | arguments: Enum.reverse(args)}, acc}

      %Ash.Query.Ref{input?: true} = ref ->
        do_replace_ref(ref, acc)

      other ->
        {other, acc}
    end
  end

  defp do_replace_ref(
         %{
           attribute: %struct{name: name},
           relationship_path: relationship_path
         } = ref,
         %{stack: [{parent, _path, action, domain} | _]} = acc
       )
       when struct in [Ash.Resource.Attribute, Ash.Resource.Aggregate, Ash.Resource.Calculation] do
    resource = Ash.Resource.Info.related(parent, relationship_path)

    action =
      case relationship_path do
        [] ->
          action

        path ->
          case Map.get(
                 Ash.Resource.Info.relationship(parent, path) || %{},
                 :read_action
               ) do
            nil ->
              Ash.Resource.Info.primary_action!(resource, :read)

            read_action ->
              Ash.Resource.Info.action(resource, read_action)
          end
      end

    expression_for_ref(resource, name, action, domain, ref, acc)
  end

  defp do_replace_ref(ref, acc) do
    {ref, acc}
  end

  defp related_with_action(resource, path) do
    first = :lists.droplast(path)
    last = List.last(path)

    relationship =
      resource
      |> Ash.Resource.Info.related(first)
      |> Ash.Resource.Info.relationship(last)

    if relationship.read_action do
      {relationship.destination,
       Ash.Resource.Info.action(relationship.destination, relationship.read_action)}
    else
      {relationship.destination,
       Ash.Resource.Info.primary_action!(relationship.destination, :read)}
    end
  end

  defp expression_for_ref(resource, field, action, domain, ref, acc) do
    case field_condition(resource, field, action, domain, acc) do
      {:none, acc} ->
        {ref, acc}

      {:expr, expr, acc} ->
        {Ash.Expr.expr(
           if ^expr do
             ^ref
           else
             nil
           end
         ), acc}
    end
  end

  defp field_condition(resource, field, action, domain, acc) do
    if Ash.Policy.Authorizer in Ash.Resource.Info.authorizers(resource) &&
         !Enum.empty?(Ash.Policy.Info.field_policies(resource)) do
      {authorizer, acc} =
        case Map.fetch(acc.authorizers, {resource, action}) do
          {:ok, authorizer} ->
            {authorizer, acc}

          :error ->
            authorizer = initial_state(acc.actor, resource, action, domain)
            query = Ash.Query.for_read(resource, action.name, actor: acc.actor)

            authorizer =
              ensure_context_in_authorizer(authorizer, %{
                query: query,
                changeset: nil,
                action_input: nil,
                domain: domain
              })

            {authorizer,
             %{acc | authorizers: Map.put(acc.authorizers, {resource, action}, authorizer)}}
        end

      {expr, authorizer} =
        if field in Ash.Resource.Info.primary_key(resource) do
          # primary keys are always accessible
          {true, authorizer}
        else
          policies = Ash.Policy.Info.field_policies_for_field(resource, field)

          case strict_check_result(
                 %{
                   authorizer
                   | policies: policies
                 },
                 for_fields: [field],
                 context_description: "accessing field in filter"
               ) do
            {:authorized, authorizer} ->
              log_successful_policy_breakdown(authorizer)
              {true, authorizer}

            {:error, _} ->
              {false, authorizer}

            {:filter, authorizer, filter} ->
              log_successful_policy_breakdown(authorizer)
              {filter, authorizer}

            {:filter_and_continue, filter, _authorizer} ->
              raise """
              Was given a partial filter for a field policy for field #{inspect(field)}.

              Filter: #{inspect(filter)}

              Field policies must currently use only filter checks or simple checks.
              """

            {:continue, _} ->
              raise """
              Detected necessity for a runtime check for a field policy for field #{inspect(field)}.

              Field policies must currently use only filter checks or simple checks.
              """
          end
        end

      new_acc = %{acc | authorizers: Map.put(acc.authorizers, {resource, action}, authorizer)}

      if expr == true do
        {:none, new_acc}
      else
        {:expr, expr,
         %{acc | authorizers: Map.put(acc.authorizers, {resource, action}, authorizer)}}
      end
    else
      {:none, acc}
    end
  end

  @impl true
  def add_calculations(query_or_changeset, authorizer, context) do
    authorizer = ensure_context_in_authorizer(authorizer, context)

    if Ash.Policy.Info.field_policies(query_or_changeset.resource) == [] do
      # If there are no field policies, access is allowed by default
      # and we don't need to add any calculations
      {:ok, query_or_changeset, authorizer}
    else
      only_public? =
        case Ash.Policy.Info.private_fields_policy(query_or_changeset.resource) do
          :include -> false
          :show -> true
          :hide -> false
        end

      accessing_fields =
        case query_or_changeset do
          %Ash.Query{} = query ->
            Ash.Query.accessing(query, [:attributes, :calculations, :aggregates], only_public?)

          %Ash.Changeset{} = changeset ->
            Ash.Changeset.accessing(
              changeset,
              [:attributes, :calculations, :aggregates],
              only_public?
            )
        end

      pkey = Ash.Resource.Info.primary_key(query_or_changeset.resource)

      accessing_fields
      # primary keys are always accessible
      |> Enum.reject(&(&1 in pkey))
      |> Enum.group_by(fn field ->
        Ash.Policy.Info.field_policies_for_field(query_or_changeset.resource, field)
      end)
      |> Enum.reduce(
        {query_or_changeset, authorizer},
        fn {policies, fields}, {query_or_changeset, authorizer} ->
          {expr, authorizer} =
            case strict_check_result(
                   %{
                     authorizer
                     | policies: policies
                   }
                   |> add_query_or_changeset(query_or_changeset),
                   for_fields: fields,
                   context_description: "selecting or loading fields"
                 ) do
              {:authorized, authorizer} ->
                log_successful_policy_breakdown(authorizer)
                {true, authorizer}

              {:error, _} ->
                {false, authorizer}

              {:filter, authorizer, filter} ->
                log_successful_policy_breakdown(authorizer)
                {filter, authorizer}

              {:filter_and_continue, filter, _authorizer} ->
                raise """
                Was given a partial filter for a field policy for fields #{inspect(fields)}.

                Filter: #{inspect(filter)}

                Field policies must currently use only filter checks or simple checks.
                """

              {:continue, _} ->
                raise """
                Detected necessity for a runtime check for a field policy for fields #{inspect(fields)}.

                Field policies must currently use only filter checks or simple checks.
                """
            end

          # This is a hack that we need to clean up.
          # Creating this kind of expression should be its own thing that we do
          # with something in the `Expr` module

          %{expression: expr} = Ash.Filter.parse!(query_or_changeset.resource, expr)

          {:ok, calculation} =
            Ash.Query.Calculation.new(
              {:__ash_fields_are_visible__, fields},
              Ash.Resource.Calculation.Expression,
              [expr: expr],
              :boolean,
              async?: false
            )

          case query_or_changeset do
            %Ash.Query{} = query ->
              {Ash.Query.load(
                 query,
                 calculation
               ), authorizer}

            %Ash.Changeset{} = query ->
              {Ash.Changeset.load(
                 query,
                 calculation
               ), authorizer}
          end
        end
      )
      |> then(fn {result, authorizer} ->
        {:ok, result, authorizer}
      end)
    end
  end

  defp ensure_context_in_authorizer(authorizer, context) do
    subject = context.query || context.changeset || context[:action_input]

    %{
      authorizer
      | query: context.query,
        changeset: context.changeset,
        action_input: context[:action_input],
        subject: subject,
        context: (subject && subject.context) || %{},
        domain: context.domain
    }
  end

  defp add_query_or_changeset(
         authorizer,
         %Ash.Query{} = query
       ),
       do: %{authorizer | query: query, subject: query}

  defp add_query_or_changeset(
         authorizer,
         %Ash.Changeset{} = changeset
       ),
       do: %{authorizer | changeset: changeset, subject: changeset}

  defp add_query_or_changeset(
         authorizer,
         _
       ),
       do: authorizer

  @doc false
  def strict_check_all_facts(authorizer) do
    case Checker.strict_check_all_facts(authorizer) do
      {:ok, authorizer, new_facts} ->
        %{authorizer | facts: new_facts}

      _ ->
        authorizer
    end
  end

  defp strict_filter(authorizer) do
    {filterable, require_check} =
      authorizer.scenarios
      |> Enum.split_with(fn scenario ->
        Enum.all?(scenario, fn {{check_module, opts}, _} ->
          opts[:access_type] == :filter ||
            match?(
              {:ok, v} when is_boolean(v),
              Ash.Policy.Policy.fetch_fact(authorizer.facts, {check_module, opts})
            )
        end)
      end)

    filter = strict_filters(filterable, authorizer)

    case {filter, require_check} do
      {[], []} ->
        {:authorized, authorizer}

      {_filters, []} ->
        if Enum.any?(filter, &(&1 == true)) do
          {:authorized, authorizer}
        else
          case filter do
            [filter] ->
              with {:ok, %Ash.Filter{expression: filter}} <-
                     Ash.Filter.parse(authorizer.resource, filter) do
                {:filter, authorizer, filter}
              end

            filters ->
              with {:ok, %Ash.Filter{expression: filter}} <-
                     Ash.Filter.parse(authorizer.resource, or: filters) do
                {:filter, authorizer, filter}
              end
          end
        end

      {_filters, _require_check} ->
        case global_filters(authorizer) do
          nil ->
            maybe_forbid_strict(authorizer)

          {filters, scenarios_without_global} ->
            with {:ok, %Ash.Filter{expression: filter}} <-
                   Ash.Filter.parse(authorizer.resource, and: filters) do
              {:filter_and_continue, filter,
               %{authorizer | check_scenarios: scenarios_without_global}}
            end
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
              nil_to_false(check_module.auto_filter(authorizer.actor, authorizer, check_opts))
            rescue
              e ->
                reraise Ash.Error.to_ash_error(e, __STACKTRACE__,
                          bread_crumbs:
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
                nil_to_false(
                  check_module.auto_filter_not(authorizer.actor, authorizer, check_opts)
                )
              else
                [
                  not:
                    nil_to_false(
                      check_module.auto_filter(authorizer.actor, authorizer, check_opts)
                    )
                ]
              end
            rescue
              e ->
                reraise Ash.Error.to_ash_error(e, __STACKTRACE__,
                          bread_crumbs:
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

  defp nil_to_false(nil), do: false
  defp nil_to_false(v), do: v

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
    {:continue, %{authorizer | check_scenarios: authorizer.scenarios}}
  end

  defp global_filters(authorizer, scenarios \\ nil, filter \\ []) do
    scenarios = scenarios || authorizer.scenarios

    global_check_value =
      Enum.find_value(scenarios, fn scenario ->
        Enum.find(scenario, fn {{check_module, _opts} = check, value} ->
          check_module.type() == :filter &&
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
            nil_to_false(check_module.auto_filter(authorizer.actor, authorizer, check_opts))
          else
            if :erlang.function_exported(check_module, :auto_filter_not, 3) do
              nil_to_false(check_module.auto_filter_not(authorizer.actor, authorizer, check_opts))
            else
              [
                not:
                  nil_to_false(check_module.auto_filter(authorizer.actor, authorizer, check_opts))
              ]
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
    {data, authorizer, any_forbidden?} =
      authorizer.data
      |> Enum.reduce({[], false, authorizer}, fn record, {data, any_forbidden?, authorizer} ->
        authorizer.scenarios
        |> Enum.reject(&scenario_impossible?(&1, authorizer, record))
        |> case do
          [] ->
            {[record | data], authorizer, any_forbidden?}

          scenarios ->
            case do_check_result(scenarios, authorizer, record) do
              {:ok, authorizer} ->
                {[record | data], authorizer, any_forbidden?}

              {:forbidden, authorizer} ->
                {data, authorizer, true}
            end
        end
      end)

    log_successful_policy_breakdown(authorizer)

    if any_forbidden? do
      {:data, Enum.reverse(data)}
    else
      :authorized
    end
  end

  defp log_successful_policy_breakdown(authorizer, filter \\ nil) do
    case Ash.Policy.Info.log_successful_policy_breakdowns() do
      nil ->
        if authorizer.subject.context[:private][:authorizer_log?] do
          do_log_successful_policy_breakdown(authorizer, filter, :info)
        else
          :ok
        end

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
            success?: true,
            help_text?: false,
            domain: authorizer.domain,
            resource: authorizer.resource,
            actor: authorizer.actor,
            subject: authorizer.subject,
            for_fields: authorizer.for_fields
          )
      ]
    )
  end

  defp do_check_result(cleaned_scenarios, authorizer, record) do
    if Enum.any?(cleaned_scenarios, &scenario_applies?(&1, authorizer, record)) do
      {:ok, authorizer}
    else
      case Enum.reject(cleaned_scenarios, &scenario_impossible?(&1, authorizer, record)) do
        [] ->
          {:forbidden, authorizer}

        scenarios ->
          check_facts_until_known(scenarios, authorizer, record)
      end
    end
  end

  defp scenario_applies?(scenario, authorizer, record) do
    Enum.all?(scenario, fn {clause, requirement} ->
      case Policy.fetch_fact(authorizer.facts, clause) do
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
        {:ok, value} when is_boolean(value) and value != requirement ->
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
    scenarios
    |> find_fact_to_check(authorizer)
    |> case do
      nil ->
        if Enum.any?(scenarios, &scenario_applies?(&1, authorizer, record)) do
          {:ok, authorizer}
        else
          {:forbidden, authorizer}
        end

      {fact, _value} ->
        new_authorizer =
          check_fact(fact, authorizer)

        if Enum.any?(scenarios, &scenario_applies?(&1, new_authorizer, record)) do
          {:ok, new_authorizer}
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
           (Ash.DataLayer.data_layer_can?(authorizer.resource, :transact) &&
              Ash.DataLayer.in_transaction?(authorizer.resource)) do
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
        not support transactions or is not currently in a transaction. This means that you have a policy
        set to `access_type :runtime` that is unsafe to be set as such. Authorization over
        create/update/destroy actions for resources that don't support transactions must only be done with
        filter and/or strict checks.
        """
      end
    end
  end

  defp find_fact_to_check(scenarios, authorizer) do
    scenarios
    |> Enum.concat()
    |> Enum.find(fn {key, _value} ->
      authorizer.facts[key] in [:unknown, nil] and not Map.has_key?(authorizer.data_facts, key)
    end)
  end

  defp strict_check_result(authorizer, opts \\ []) do
    %{authorizer | for_fields: opts[:for_fields]}
    |> Checker.strict_check_scenarios()
    |> handle_strict_check_result(opts)
  end

  defp handle_strict_check_result({:ok, true, authorizer}, _opts), do: {:authorized, authorizer}

  defp handle_strict_check_result({:ok, none, authorizer}, opts) when none in [false, []] do
    handle_strict_check_result({:error, authorizer, :unsatisfiable}, opts)
  end

  defp handle_strict_check_result({:ok, scenarios, authorizer}, _opts) do
    case Checker.find_real_scenarios(scenarios, authorizer.facts) do
      [] ->
        maybe_strict_filter(authorizer, scenarios)

      _real_scenarios ->
        {:authorized, authorizer}
    end
  end

  defp handle_strict_check_result({:error, authorizer, :unsatisfiable}, opts) do
    if authorizer.action.type == :action || Enum.empty?(authorizer.policies || []) do
      {:error,
       Ash.Error.Forbidden.Policy.exception(
         facts: authorizer.facts,
         domain: Map.get(authorizer, :domain),
         solver_statement: Map.get(authorizer, :solver_statement),
         policies: authorizer.policies,
         subject: authorizer.subject,
         context_description: opts[:context_description],
         for_fields: opts[:for_fields],
         resource: Map.get(authorizer, :resource),
         action: Map.get(authorizer, :action),
         actor: Map.get(authorizer, :actor),
         scenarios: []
       )}
    else
      if forbidden_due_to_strict_policy?(authorizer) do
        {:error,
         Ash.Error.Forbidden.Policy.exception(
           facts: authorizer.facts,
           domain: Map.get(authorizer, :domain),
           solver_statement: Map.get(authorizer, :solver_statement),
           policies: authorizer.policies,
           subject: authorizer.subject,
           context_description: opts[:context_description],
           for_fields: opts[:for_fields],
           resource: Map.get(authorizer, :resource),
           action: Map.get(authorizer, :action),
           actor: Map.get(authorizer, :actor),
           scenarios: []
         )}
      else
        {:filter, authorizer, false}
      end
    end
  end

  defp handle_strict_check_result({:error, _authorizer, exception}, _opts) do
    {:error, Ash.Error.to_ash_error(exception)}
  end

  defp maybe_strict_filter(authorizer, scenarios) do
    strict_filter(%{authorizer | scenarios: scenarios})
  end

  defp forbidden_due_to_strict_policy?(authorizer) do
    no_filter_static_forbidden_reads? =
      Keyword.get(
        Application.get_env(:ash, :policies, []),
        :no_filter_static_forbidden_reads?,
        true
      )

    if no_filter_static_forbidden_reads? || authorizer.for_fields ||
         authorizer.action.type != :read do
      true
    else
      if Enum.any?(authorizer.policies, fn policy ->
           Enum.all?(policy.condition || [], fn {check_module, check_opts} ->
             Policy.fetch_fact(authorizer.facts, {check_module, check_opts}) in [
               {:ok, true},
               :unknown
             ]
           end)
         end) do
        authorizer.policies
        |> Enum.any?(fn policy ->
          policy.access_type == :strict and
            Enum.all?(policy.condition || [], fn {check_module, check_opts} ->
              Policy.fetch_fact(authorizer.facts, {check_module, check_opts}) == {:ok, true}
            end) and
            policy_fails_statically?(authorizer, policy)
        end)
      else
        true
      end
    end
  end

  defp policy_fails_statically?(authorizer, policy) do
    Enum.reduce_while(policy.policies, :forbidden, fn check, status ->
      case check.type do
        :authorize_if ->
          if Policy.fetch_fact(authorizer.facts, {check.check_module, check.check_opts}) ==
               {:ok, true} do
            {:halt, :authorized}
          else
            {:cont, status}
          end

        :forbid_if ->
          if Policy.fetch_fact(authorizer.facts, {check.check_module, check.check_opts}) ==
               {:ok, true} do
            {:halt, :forbidden}
          else
            {:cont, status}
          end

        :authorize_unless ->
          if Policy.fetch_fact(authorizer.facts, {check.check_module, check.check_opts}) ==
               {:ok, true} do
            {:cont, status}
          else
            {:halt, :authorized}
          end

        :forbid_unless ->
          if Policy.fetch_fact(authorizer.facts, {check.check_module, check.check_opts}) ==
               {:ok, true} do
            {:cont, status}
          else
            {:halt, :forbidden}
          end
      end
    end)
    |> Kernel.==(:forbidden)
  end

  defp get_policies(authorizer) do
    %{
      authorizer
      | policies: Ash.Policy.Info.policies(authorizer.domain, authorizer.resource)
    }
  end

  def expr_check(expr) do
    {:ok, {Ash.Policy.Check.Expression, expr: expr}}
  end

  @templates [
    :_actor,
    :_arg,
    :_ref,
    :_combination,
    :_parent,
    :_atomic_ref,
    :_context
  ]

  def template_var({template_var, _} = expr)
      when template_var in @templates do
    {:ok, {Ash.Policy.Check.Expression, expr: expr}}
  end

  def template_var({template_var, _, _} = expr)
      when template_var in @templates do
    {:ok, {Ash.Policy.Check.Expression, expr: expr}}
  end

  def template_var(_), do: {:error, "not a template var"}
end
