defmodule Ash.Authorization.Rule do
  defstruct [:kind, :check, :describe, :precheck]

  @type kind :: :allow | :allow_unless | :allow_only | :deny | :deny_unless | :deny_only
  @type user :: Ash.user() | nil
  @type data :: list(Ash.resource())
  @type context :: %{
          required(:resource) => Ash.resource(),
          required(:action) => Ash.action(),
          required(:params) => Ash.params(),
          optional(atom) => term
        }
  @type resource_ids :: list(term)

  # Required sideloads before checks are run
  @type side_load_instruction :: {:side_load, Ash.side_load()}
  # The result for this check is predetermined for all records
  # that could be passed in from this request.
  @type precheck_instruction :: {:precheck, boolean}
  @type precheck_context :: {:context, %{optional(atom) => term}}
  @type precheck_result :: side_load_instruction() | precheck_instruction() | precheck_context()

  @type check :: {module, atom, list(term)}
  @type precheck :: {module, atom, list(term)}
  @type describe :: String.t()
  @type rule_options :: Keyword.t()

  @type t() :: %__MODULE__{
          kind: kind(),
          check: check(),
          describe: describe(),
          precheck: precheck() | nil
        }

  @kinds [
    :allow,
    :allow_unless,
    :allow_only,
    :deny,
    :deny_unless,
    :deny_only
  ]

  @builtin_checks %{
    relationship_access: Ash.Authorization.Check.RelationshipAccess,
    static: Ash.Authorization.Check.Static,
    user_field: Ash.Authorization.Check.UserField
  }

  @builtin_check_names Map.keys(@builtin_checks)

  @doc false
  def kinds(), do: @kinds

  for kind <- @kinds do
    def unquote(kind)(opts) do
      new(unquote(kind), opts)
    end

    def unquote(kind)(check, opts) do
      new(unquote(kind), {check, opts})
    end
  end

  def new({kind, opts}), do: new(kind, opts)

  def new(kind, opts) when kind not in @kinds do
    raise "Invalid rule declaration: #{kind}: #{inspect(opts)}"
  end

  def new(kind, module) when is_atom(module) do
    new(kind, {module, []})
  end

  def new(kind, {name, opts}) when name in @builtin_check_names() do
    new(kind, {Map.get(@builtin_checks, name), opts})
  end

  def new(kind, {check_module, opts}) when is_list(opts) and is_atom(check_module) do
    case check_module.init(opts) do
      {:ok, opts} ->
        new(kind,
          check: {check_module, :check, [opts]},
          describe: check_module.describe(opts),
          precheck: {check_module, :precheck, [opts]}
        )

      {:error, error} ->
        # TODO: nicer
        raise error
    end
  end

  def new(kind, opts) when is_list(opts) do
    struct!(__MODULE__, Keyword.put(opts, :kind, kind))
  end

  def run_check(
        %{check: check, kind: kind},
        user,
        data,
        context
      ) do
    check_function =
      case check do
        {module, function, args} ->
          fn user, data, context ->
            apply(module, function, [user, data, context] ++ args)
          end

        function ->
          function
      end

    result = check_function.(user, data, context)

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

  @spec result_to_decision(kind(), boolean()) :: Authorizer.result()
  def result_to_decision(:allow, true), do: :allow
  def result_to_decision(:allow, false), do: :undecided

  def result_to_decision(:allow_only, true), do: :allow
  def result_to_decision(:allow_only, false), do: :unauthorized

  def result_to_decision(:allow_unless, true), do: :undecided
  def result_to_decision(:allow_unless, false), do: :allow

  def result_to_decision(:deny, true), do: :unauthorized
  def result_to_decision(:deny, false), do: :undecided

  def result_to_decision(:deny_only, true), do: :unauthorized
  def result_to_decision(:deny_only, false), do: :allow

  def result_to_decision(:deny_unless, true), do: :undecided
  def result_to_decision(:deny_unless, false), do: :unauthorized
end
