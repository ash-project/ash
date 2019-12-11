defmodule Ash.Authorization.Rule do
  defstruct [:kind, :check, :describe, :precheck]

  @type kind :: :forbid_if | :forbid_unless | :approve_if | :approve_unless
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
  @type precheck_instruction :: {:decision, boolean}
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

  def new(kind, check_module, opts) when is_atom(check_module) do
    new(kind,
      check: {check_module, :check, [opts]},
      describe: check_module.describe(opts),
      precheck: {check_module, :precheck, [opts]}
    )
  end

  def new(kind, opts) when is_list(opts) do
    {:ok, struct(__MODULE__, Keyword.put(opts, :kind, kind))}
  end

  @spec result_to_decision(kind(), boolean()) :: Authorizer.result()
  def result_to_decision(:allow_if, true), do: :allowed
  def result_to_decision(:allow_if, false), do: :undecided

  def result_to_decision(:allow_unless, true), do: :undecided
  def result_to_decision(:allow_unless, false), do: :allowed

  def result_to_decision(:forbid_if, true), do: :forbidden
  def result_to_decision(:forbid_if, false), do: :undecided

  def result_to_decision(:forbid_unless, true), do: :undecided
  def result_to_decision(:forbid_unless, false), do: :forbidden
end
