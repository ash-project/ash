defmodule Ash.Policy.Info do
  @moduledoc """
  An authorization extension for ash resources.

  For more information, see `Ash.Policy.Authorizer`
  """

  alias Spark.Dsl.Extension

  @doc "Whether or not Ash policy authorizer is configured to show policy breakdowns in error messages"
  def show_policy_breakdowns? do
    Keyword.get(Application.get_env(:ash, :policies, []), :show_policy_breakdowns?, false)
  end

  @doc "Whether or not Ash policy authorizer is configured to log policy breakdowns"
  def log_policy_breakdowns do
    Application.get_env(:ash, :policies)[:log_policy_breakdowns]
  end

  @doc "Whether or not Ash policy authorizer is configured to log successful policy breakdowns"
  def log_successful_policy_breakdowns do
    Application.get_env(:ash, :policies)[:log_successful_policy_breakdowns]
  end

  @doc "Gets the field policies relevant to a given field"
  def field_policies_for_field(resource, field) do
    Extension.get_persisted(resource, :fields_to_field_policies, %{})[field]
  end

  @doc """
  A utility to determine if a given query/changeset would pass authorization.

  *This is still experimental.*
  """
  def strict_check(_actor, %{action: nil}, _) do
    raise "Cannot use `strict_check/3` unless an action has been set on the query/changeset"
  end

  def strict_check(actor, %{__struct__: Ash.Query} = query, domain) do
    query = Ash.Query.set_context(query, %{private: %{pre_flight_authorization?: true}})

    authorizer = %Ash.Policy.Authorizer{
      actor: actor,
      resource: query.resource,
      action: query.action
    }

    case Ash.Policy.Authorizer.strict_check(authorizer, %{
           domain: domain,
           query: query,
           changeset: nil
         }) do
      {:error, _error} ->
        false

      {:authorized, _} ->
        true

      {:filter, _, _} ->
        true

      _ ->
        :maybe
    end
  end

  def strict_check(actor, %{__struct__: Ash.Changeset} = changeset, domain) do
    changeset =
      Ash.Changeset.set_context(changeset, %{private: %{pre_flight_authorization?: true}})

    authorizer = %Ash.Policy.Authorizer{
      actor: actor,
      resource: changeset.resource,
      action: changeset.action
    }

    case Ash.Policy.Authorizer.strict_check(authorizer, %{
           domain: domain,
           changeset: changeset,
           query: nil
         }) do
      {:error, _error} ->
        false

      {:authorized, _} ->
        true

      {:filter, _, _} ->
        :maybe

      _ ->
        :maybe
    end
  end

  def describe_resource(domain, resource) do
    domain
    |> policies(resource)
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

  def field_policies(resource) do
    resource
    |> Extension.get_entities([:field_policies])
    |> set_access_type(default_access_type(resource))
  end

  @private_fields_policy_default Application.compile_env(:ash, :policies)[:private_fields] ||
                                   :show

  def private_fields_policy(resource) do
    Extension.get_opt(
      resource,
      [:field_policies],
      :private_fields,
      @private_fields_policy_default
    )
  end

  def policies(domain, resource) do
    if domain do
      do_policies(domain) ++ do_policies(resource)
    else
      do_policies(resource)
    end
  end

  defp do_policies(resource) do
    resource
    |> Extension.get_entities([:policies])
    |> flatten_groups()
    |> set_access_type(default_access_type(resource))
  end

  defp flatten_groups(policies) do
    Enum.flat_map(policies, fn
      %Ash.Policy.Policy{} = policy ->
        [policy]

      %Ash.Policy.PolicyGroup{condition: condition, policies: policies} ->
        policies
        |> flatten_groups()
        |> Enum.map(fn policy ->
          %{policy | condition: List.wrap(condition) ++ List.wrap(policy.condition)}
        end)
    end)
  end

  def default_access_type(resource) do
    Extension.get_opt(resource, [:policies], :default_access_type, :filter, false)
  end

  # This should be done at compile time
  defp set_access_type(policies, default) when is_list(policies) do
    Enum.map(policies, &set_access_type(&1, default))
  end

  defp set_access_type(
         %Ash.Policy.Policy{
           policies: policies,
           condition: conditions,
           access_type: access_type
         } = policy,
         default
       ) do
    %{
      policy
      | policies: set_access_type(policies, access_type || default),
        condition: set_access_type(conditions, access_type || default),
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
