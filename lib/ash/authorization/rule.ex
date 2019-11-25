defmodule Ash.Authorization.Rule do
  defstruct [:kind, :check, :describe, :extra_context, :precheck, batch?: true]

  @kinds [
    :allow,
    :allow_unless,
    :allow_only,
    :deny,
    :deny_unless,
    :deny_only
  ]

  def allow(opts), do: new(:allow, opts)
  def allow_unless(opts), do: new(:allow_unless, opts)
  def allow_only(opts), do: new(:allow_only, opts)
  def deny(opts), do: new(:deny, opts)
  def deny_unless(opts), do: new(:deny_unless, opts)
  def deny_only(opts), do: new(:deny_only, opts)

  def new({left, right}), do: new(left, right)

  def new(:owner_relationship, relationship) do
    new(:allow,
      check: &Ash.Authorization.BuiltIn.owner_relationship/3,
      describe: "the current user is the `#{relationship}`",
      extra_context: %{relationship: relationship},
      precheck: &Ash.Authorization.BuiltIn.owner_relationship_precheck/2
    )
  end

  def new(kind, opts) when kind not in @kinds do
    raise "Invalid rule declaration: #{kind}: #{inspect(opts)}"
  end

  def new(kind, opts) do
    struct!(__MODULE__, Keyword.put(opts, :kind, kind))
  end

  def run_check(
        %{batch?: false, check: check, extra_context: extra_context, kind: kind},
        user,
        data,
        context
      ) do
    Enum.map(data, fn item ->
      result = check.(user, data, Map.merge(extra_context, context))
      decision = result_to_decision(kind, result)

      Map.put(item, :__authorization_decision__, decision)
    end)
  end

  def run_check(
        %{check: check, extra_context: extra_context, kind: kind},
        user,
        data,
        context
      ) do
    result = check.(user, data, Map.merge(extra_context, context))

    Enum.map(data, fn item ->
      result =
        case result do
          true -> true
          false -> false
          ids -> item.id in ids
        end

      decision = result_to_decision(kind, result)
      Map.put(item, :__authorization_decision__, decision)
    end)
  end

  def result_to_decision(:allow, true), do: :allow
  def result_to_decision(:allow, false), do: :undecided

  def result_to_decision(:allow_only, true), do: :allow
  def result_to_decision(:allow_only, false), do: :unauthorizez

  def result_to_decision(:allow_unless, true), do: :undecided
  def result_to_decision(:allow_unless, false), do: :allow

  def result_to_decision(:deny, true), do: :unauthorized
  def result_to_decision(:deny, false), do: :undecided

  def result_to_decision(:deny_only, true), do: :unauthorized
  def result_to_decision(:deny_only, false), do: :allow

  def result_to_decision(:deny_unless, true), do: :undecided
  def result_to_decision(:deny_unless, false), do: :unauthorized
end
