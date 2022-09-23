defmodule Ash.Policy.Info do
  @moduledoc """
  An authorization extension for ash resources.

  For more information, see `Ash.Policy.Authorizer`
  """
  @type request :: Ash.Engine.Request.t()

  alias Spark.Dsl.Extension

  @doc "Whether or not ash policy authorizer is configured to show policy breakdowns in error messages"
  def show_policy_breakdowns? do
    Application.get_env(:ash, :policies)[:show_policy_breakdowns?] || false
  end

  @doc "Whether or not ash policy authorizer is configured to show policy breakdowns in error messages"
  def log_policy_breakdowns do
    Application.get_env(:ash, :policies)[:log_policy_breakdowns]
  end

  @doc """
  A utility to determine if a given query/changeset would pass authorization.

  *This is still experimental.*
  """
  def strict_check(_actor, %{action: nil}, _) do
    raise "Cannot use `strict_check/3` unless an action has been set on the query/changeset"
  end

  def strict_check(actor, %Ash.Query{} = query, api) do
    authorizer = %Ash.Policy.Authorizer{
      actor: actor,
      resource: query.resource,
      action: query.action
    }

    case Ash.Policy.Authorizer.strict_check(authorizer, %{
           api: api,
           query: query,
           changeset: nil
         }) do
      {:error, _error} ->
        false

      :authorized ->
        true

      {:filter, _, _} ->
        true

      _ ->
        :maybe
    end
  end

  def strict_check(actor, %Ash.Changeset{} = changeset, api) do
    authorizer = %Ash.Policy.Authorizer{
      actor: actor,
      resource: changeset.resource,
      action: changeset.action
    }

    case Ash.Policy.Authorizer.strict_check(authorizer, %{
           api: api,
           changeset: changeset,
           query: nil
         }) do
      {:error, _error} ->
        false

      :authorized ->
        true

      {:filter, _, _} ->
        :maybe

      _ ->
        :maybe
    end
  end

  def describe_resource(resource) do
    resource
    |> policies()
    |> describe_policies()
  end

  defp describe_policies(policies) do
    Enum.map_join(policies, "\n", fn policy ->
      case policy.condition do
        empty when empty in [nil, []] ->
          describe_checks(policy.policies)

        conditions ->
          "When:\n" <>
            indent(describe_conditions(conditions)) <>
            "\nThen:\n" <> indent(describe_checks(policy.policies))
      end
    end)
  end

  defp describe_checks(checks) do
    checks
    |> Enum.map_join("\n", fn
      %{type: type, check_module: check_module, check_opts: check_opts} ->
        "#{type}: #{check_module.describe(check_opts)}"
    end)
    |> Kernel.<>("\n")
  end

  defp describe_conditions(conditions) do
    Enum.map_join(conditions, " and ", fn
      {check_module, check_opts} ->
        check_module.describe(check_opts)
    end)
  end

  defp indent(string) do
    string
    |> String.split("\n")
    |> Enum.map_join("\n", fn line ->
      "  " <> line
    end)
  end

  def policies(resource) do
    resource
    |> Extension.get_entities([:policies])
    |> set_access_type(default_access_type(resource))
  end

  def default_access_type(resource) do
    Extension.get_opt(resource, [:policies], :default_access_type, :strict, false)
  end

  @doc """
  A utility to determine if an actor is or may be authorized for a given action.

  A shortcut for calling `can/4` but with the `maybe_is` option defaulting to `false`, so this should always return a boolean.

  See the documentation of `can/4` for more.
  """
  @type can_option? :: {:api, module} | {:maybe_is, boolean()}
  @spec can?(Ash.Resource.t(), atom(), map() | nil, list(can_option?())) :: boolean()
  def can?(resource, action_or_action_name, actor, opts \\ []) do
    opts = Keyword.put(opts, :maybe_is, Keyword.get(opts, :maybe_is, false))

    can(resource, action_or_action_name, actor, opts)
  end

  @doc """
  A utility to determine if an actor is or may be authorized for a given action.

  This only runs the "strict check" portion of policies, meaning that it can return `:maybe` in some cases.
  If you have `access_type :runtime` in any of your policies, then you may get `:maybe` from this function.
  To customize what is returned in the case of `:maybe` you can provide the `maybe_is` option, i.e `maybe_is: true`.
  This makes sense when you want to a show a button, but only if the user may be able to perform the action.

  For read actions, an important thing to factor in here is that typically policies just end up filtering the action.
  This means that even if you try to read something you can't read, your read action will succeed but nothing will be
  returned, and this function would return `true`.
  """
  @type can_option :: {:api, module} | {:maybe_is, boolean() | :maybe}
  @spec can(Ash.Resource.t(), atom(), map() | nil, list(can_option())) :: boolean() | :maybe
  def can(resource, action_or_action_name, actor, opts \\ []) do
    api = Keyword.fetch!(opts, :api)
    maybe_is = Keyword.get(opts, :maybe_is, :maybe)

    action =
      case action_or_action_name do
        %Ash.Resource.Actions.Create{} = action -> action
        %Ash.Resource.Actions.Read{} = action -> action
        %Ash.Resource.Actions.Update{} = action -> action
        %Ash.Resource.Actions.Destroy{} = action -> action
        name when is_atom(name) -> Ash.Resource.Info.action(resource, name)
      end

    # Get action type from resource
    case action.type do
      :update ->
        query =
          struct(resource)
          |> Ash.Changeset.new(%{})
          |> Ash.Changeset.for_update(action.name)

        run_check(actor, query, api: api, maybe_is: maybe_is)

      :create ->
        query =
          resource
          |> Ash.Changeset.new()
          |> Ash.Changeset.for_create(action.name)

        run_check(actor, query, api: api, maybe_is: maybe_is)

      :read ->
        query = Ash.Query.for_read(resource, action.name)
        run_check(actor, query, api: api, maybe_is: maybe_is)

      :destroy ->
        query =
          struct(resource)
          |> Ash.Changeset.new()
          |> Ash.Changeset.for_destroy(action.name)

        run_check(actor, query, api: api, maybe_is: maybe_is)

      action_type ->
        raise ArgumentError, message: "Invalid action type \"#{action_type}\""
    end
  end

  defp run_check(actor, query, api: api, maybe_is: maybe_is) do
    case Ash.Policy.Info.strict_check(actor, query, api) do
      true ->
        true

      :maybe ->
        maybe_is

      _ ->
        false
    end
  end

  # This should be done at compile time
  defp set_access_type(policies, default) when is_list(policies) do
    Enum.map(policies, &set_access_type(&1, default))
  end

  defp set_access_type(
         %Ash.Policy.Policy{
           policies: policies,
           condition: conditions,
           checks: checks,
           access_type: access_type
         } = policy,
         default
       ) do
    %{
      policy
      | policies: set_access_type(policies, default),
        condition: set_access_type(conditions, default),
        checks: set_access_type(checks, default),
        access_type: access_type || default
    }
  end

  defp set_access_type(%Ash.Policy.Check{check_opts: check_opts} = check, default) do
    %{check | check_opts: Keyword.update(check_opts, :access_type, default, &(&1 || default))}
  end

  defp set_access_type({module, opts}, default),
    do: {module, Keyword.update(opts, :access_type, default, &(&1 || default))}

  defp set_access_type(other, _), do: other
end
