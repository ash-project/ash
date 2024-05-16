defmodule Ash.Type do
  @array_constraints [
    min_length: [
      type: :non_neg_integer,
      doc: "A minimum length for the items."
    ],
    items: [
      type: :any,
      doc: "A schema for individual items."
    ],
    max_length: [
      type: :non_neg_integer,
      doc: "A maximum length for the items."
    ],
    nil_items?: [
      type: :boolean,
      doc: "Whether or not the list can contain nil items.",
      default: false
    ],
    remove_nil_items?: [
      type: :boolean,
      doc: "Whether or not to remove the nil items from the list instead of adding errors.",
      default: false
    ],
    empty_values: [
      type: {:list, :any},
      doc: "A set of values that, if encountered, will be considered an empty list.",
      default: [""]
    ]
  ]

  @builtin_short_names [
    map: Ash.Type.Map,
    keyword: Ash.Type.Keyword,
    term: Ash.Type.Term,
    atom: Ash.Type.Atom,
    string: Ash.Type.String,
    integer: Ash.Type.Integer,
    float: Ash.Type.Float,
    duration_name: Ash.Type.DurationName,
    function: Ash.Type.Function,
    boolean: Ash.Type.Boolean,
    struct: Ash.Type.Struct,
    uuid: Ash.Type.UUID,
    binary: Ash.Type.Binary,
    date: Ash.Type.Date,
    time: Ash.Type.Time,
    decimal: Ash.Type.Decimal,
    ci_string: Ash.Type.CiString,
    naive_datetime: Ash.Type.NaiveDatetime,
    utc_datetime: Ash.Type.UtcDatetime,
    utc_datetime_usec: Ash.Type.UtcDatetimeUsec,
    datetime: Ash.Type.DateTime,
    url_encoded_binary: Ash.Type.UrlEncodedBinary,
    union: Ash.Type.Union,
    module: Ash.Type.Module,
    vector: Ash.Type.Vector
  ]

  @custom_short_names Application.compile_env(:ash, :custom_types, [])

  @short_names @custom_short_names ++ @builtin_short_names

  @doc_array_constraints Keyword.put(@array_constraints, :items,
                           type: :any,
                           doc:
                             "Constraints for the elements of the list. See the contained type's docs for more."
                         )
  @moduledoc """
  The `Ash.Type` behaviour is used to define a value type in Ash.

  ## Built in types

  #{Enum.map_join(@builtin_short_names, fn {key, module} -> "* `#{inspect(key)}` - `#{inspect(module)}`\n" end)}

  ## Lists/Arrays

  To specify a list of values, use `{:array, Type}`. Arrays are special, and have special constraints:

  #{Spark.Options.docs(@doc_array_constraints)}

  ## Defining Custom Types

  Generally you add `use Ash.Type` to your module (it is possible to add `@behaviour
  Ash.Type` and define everything yourself, but this is more work and error-prone).

  Overriding the `{:array, type}` behaviour. By defining the `*_array` versions
  of `cast_input`, `cast_stored`, `dump_to_native` and `apply_constraints`, you can
  override how your type behaves as a collection. This is how the features of embedded
  resources are implemented. No need to implement them unless you wish to override the
  default behaviour. Your type is responsible for handling nil values in each callback as well.

  Simple example of a float custom type

  ```Elixir
  defmodule GenTracker.AshFloat do
    use Ash.Type

    @impl Ash.Type
    def storage_type(_), do: :float

    @impl Ash.Type
    def cast_input(nil, _), do: {:ok, nil}
    def cast_input(value, _) do
      Ecto.Type.cast(:float, value)
    end

    @impl Ash.Type
    def cast_stored(nil, _), do: {:ok, nil}
    def cast_stored(value, _) do
      Ecto.Type.load(:float, value)
    end

    @impl Ash.Type
    def dump_to_native(nil, _), do: {:ok, nil}
    def dump_to_native(value, _) do
      Ecto.Type.dump(:float, value)
    end
  end
  ```

  All the Ash built-in types are implemented with `use Ash.Type` so they are good
  examples to look at to create your own `Ash.Type`.

  ### Short names

  You can define short `:atom_names` for your custom types by adding them to your Ash configuration:

  ```Elixir
  config :ash, :custom_types, [ash_float: GenTracker.AshFloat]
  ```

  Doing this will require a recompilation of the `:ash` dependency which can be triggered by calling:

  ```bash
  $ mix deps.compile ash --force
  ```

  ## Composite Types

  Composite types are composite *in the data layer*. Many data layers do not support this, but some (like AshPostgres),
  do. To define a composite type, the following things should be true:

  1. A casted value should be a map or struct, for example for a point: `%{x: 1, y: 2}`
  2. The data layer must support composite types, and the data layer representation will be a tuple, i.e `{1, 2}`
  3. Define `def composite?(_), do: true` in your composite type
  4. Define the type & constraints of each item in the tuple, and its name in the map
     representation: `def composite_types(_), do: [{:x, :integer, []}, {:y, :integer, []}]`.
     You can also define a storage key for each item in the tuple, if the underlying type implementation
     has a different reference for an item, i.e `def composite_types(_), do: [{:x, :x_coord, :integer, []}, {:y, :y_coord, :integer, []}]`

  With the above implemented, your composite type can be used in expressions, for example:

  ```elixir
  Ash.Query.filter(expr(coordinates[:x] == 1))k
  ```

  And you can also *construct* composite types in expressions, for example:

  ```elixir
  calculate :coordinates, :composite_point, expr(composite_type(%{x: some_value, y: some_other_value}, Point))
  ```

  ## Constraints

  Constraints are a way of validating an input type. This validation can be used in both attributes and arguments. The kinds of constraints you can apply depends on the type the data. You can find all types in `Ash.Type` . Each type has its own page on which the available constraints are listed. For example in `Ash.Type.String` you can find 5 constraints:

  - `:max_length`
  - `:min_length`
  - `:match`
  - `:trim?`
  - `:allow_empty?`

  You can also discover these constraints from iex:

  ```bash
  $ iex -S mix
  iex(1)> Ash.Type.String.constraints
  [
    max_length: [
      type: :non_neg_integer,
      doc: "Enforces a maximum length on the value"
    ],
    min_length: [
      type: :non_neg_integer,
      doc: "Enforces a minimum length on the value"
    ],
    match: [
      type: {:custom, Ash.Type.String, :match, []},
      doc: "Enforces that the string matches a passed in regex"
    ],
    trim?: [type: :boolean, doc: "Trims the value.", default: true],
    allow_empty?: [
      type: :boolean,
      doc: "If false, the value is set to `nil` if it's empty.",
      default: false
    ]
  ]
  ```

  ### Attribute example

  To show how constraints can be used in a attribute, here is an example attribute describing a username:

  ```elixir
  defmodule MyProject.MyDomain.Account do
    # ...

    code_interface do
      define :create, action: :create
    end

    actions do
      default [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :username, :string do
        constraints [
          max_length: 20,
          min_length: 3,
          match: ~r/^[a-z_-]*$/,
          trim?: true,
          allow_empty?: false
        ]
      end
    end

    # ...
  end
  ```

  If, when creating or updating this attribute, one of the constraints are not met, an error will be given telling you which constraint was broken. See below:

  ```elixir
  iex(1)> MyProject.MyDomain.Account.create!(%{username: "hi"})

  ** (Ash.Error.Invalid) Invalid Error

  * Invalid value provided for username: length must be greater than or equal to 3.

  "hi"

  iex(2)> MyProject.MyDomain.Account.create!(%{username: "Hello there this is a long string"})

  ** (Ash.Error.Invalid) Invalid Error

  * Invalid value provided for username: length must be less than or equal to 20.

  "Hello there this is a long string"

  iex(3)> MyProject.MyDomain.Account.create!(%{username: "hello there"})
  ** (Ash.Error.Invalid) Invalid Error

  * Invalid value provided for username: must match the pattern ~r/^[a-z_-]*$/.

  "hello there"

  iex(4)> MyProject.MyDomain.Account.create!(%{username: ""})
  ** (Ash.Error.Invalid) Invalid Error

  * attribute title is required
  ```

  It will give you the resource as usual on successful requests:

  ```elixir
  iex(5)> MyProject.MyDomain.Account.create!(%{username: "hello"})
  #MyProject.MyDomain.Account<
    __meta__: #Ecto.Schema.Metadata<:loaded, "account">,
    id: "7ba467dd-277c-4916-88ae-f62c93fee7a3",
    username: "hello",
    ...
  >
  ```
  """

  @type constraints :: Keyword.t()
  @type constraint_error :: String.t() | {String.t(), Keyword.t()}
  @type t :: atom | {:array, atom}
  @type error :: :error | {:error, String.t() | Keyword.t()}

  @type load_context :: %{
          domain: Ash.Domain.t(),
          actor: term() | nil,
          tenant: term(),
          tracer: list(Ash.Tracer.t()) | Ash.Tracer.t() | nil,
          authorize?: boolean | nil
        }

  @type merge_load_context :: %{
          domain: Ash.Domain.t(),
          calc_name: term(),
          calc_load: term(),
          calc_path: list(atom),
          reuse_values?: boolean,
          strict_loads?: boolean,
          initial_data: term(),
          relationship_path: list(atom)
        }

  @callback storage_type() :: Ecto.Type.t()
  @callback storage_type(constraints) :: Ecto.Type.t()
  @callback include_source(constraints, Ash.Changeset.t()) :: constraints

  @doc """
  A map of operators with overloaded implementations.

  These will only be honored if the type is placed in `config :ash, :known_types, [...Type]`

  A corresponding `evaluate_operator/1` clause should match.
  """
  @callback operator_overloads() :: %{optional(atom) => %{optional(term) => module()}}

  @doc """
  The implementation for any overloaded implementations.
  """
  @callback evaluate_operator(term) :: {:known, term} | :unknown | {:error, term()}

  @doc """
  Useful for typed data layers (like ash_postgres) to instruct them not to attempt to cast input values.

  You generally won't need this, but it can be an escape hatch for certain cases.
  """
  @callback init(constraints) :: {:ok, constraints} | {:error, Ash.Error.t()}
  @callback cast_in_query?(constraints) :: boolean
  @callback can_load?(constraints) :: boolean
  @callback ecto_type() :: Ecto.Type.t()
  @callback cast_input(term, constraints) ::
              {:ok, term} | error()
  @callback cast_input_array(list(term), constraints) :: {:ok, list(term)} | error()
  @callback cast_stored(term, constraints) :: {:ok, term} | error()
  @callback cast_stored_array(list(term), constraints) ::
              {:ok, list(term)} | error()
  @callback dump_to_native(term, constraints) :: {:ok, term} | error()
  @callback dump_to_native_array(list(term), constraints) :: {:ok, term} | error()
  @callback dump_to_embedded(term, constraints) :: {:ok, term} | :error
  @callback dump_to_embedded_array(list(term), constraints) :: {:ok, term} | error()
  @callback handle_change(old_term :: term, new_term :: term, constraints) ::
              {:ok, term} | error()
  @callback composite?(constraints) :: boolean
  @callback composite_types(constraints) ::
              list({name, type, constraints} | {name, storage_key, type, constraints})
            when name: atom, type: t, storage_key: atom
  @callback handle_change_array(old_term :: list(term), new_term :: list(term), constraints) ::
              {:ok, term} | error()
  @callback prepare_change(old_term :: term, new_uncasted_term :: term, constraints) ::
              {:ok, term} | error()
  @callback prepare_change_array(
              old_term :: list(term),
              new_uncasted_term :: list(term),
              constraints
            ) ::
              {:ok, term} | error()

  @callback constraints() :: constraints()
  @callback array_constraints() :: constraints()
  @callback apply_constraints(term, constraints) ::
              {:ok, new_value :: term}
              | :ok
              | {:error, constraint_error() | list(constraint_error)}
  @callback apply_constraints_array(list(term), constraints) ::
              {:ok, new_values :: list(term)}
              | :ok
              | {:error, constraint_error() | list(constraint_error)}
  @callback describe(constraints()) :: String.t() | nil
  @callback equal?(term, term) :: boolean
  @callback embedded?() :: boolean
  @callback generator(constraints) :: Enumerable.t()
  @callback simple_equality?() :: boolean
  @callback cast_atomic(new_value :: Ash.Expr.t(), constraints) ::
              {:atomic, Ash.Expr.t()} | {:error, Ash.Error.t()} | {:not_atomic, String.t()}
  @callback cast_atomic_array(new_value :: Ash.Expr.t(), constraints) ::
              {:atomic, Ash.Expr.t()} | {:error, Ash.Error.t()} | {:not_atomic, String.t()}
  @callback custom_apply_constraints_array?() :: boolean
  @callback load(
              values :: list(term),
              load :: Keyword.t(),
              constraints :: Keyword.t(),
              context :: load_context()
            ) ::
              {:ok, list(term)} | {:error, Ash.Error.t()}

  @callback loaded?(
              value :: term,
              path_to_load :: list(atom),
              constraints :: Keyword.t(),
              opts :: Keyword.t()
            ) :: boolean

  @callback merge_load(
              left :: term,
              right :: term,
              constraints :: Keyword.t(),
              context :: merge_load_context() | nil
            ) ::
              {:ok, term} | {:error, error} | :error

  @type rewrite_data ::
          {type :: :calc | :agg, rewriting_name :: atom, rewriting_load :: atom}
          | {:rel, rewriting_name :: atom}
  @type rewrite :: {{list(atom), rewrite_data, atom, atom}, source :: term}

  @callback get_rewrites(
              merged_load :: term,
              calculation :: Ash.Query.Calculation.t(),
              path :: list(atom),
              constraints :: Keyword.t()
            ) :: [rewrite]
  @callback rewrite(value :: term, [rewrite], constraints :: Keyword.t()) :: value :: term
  @callback prepare_change_array?() :: boolean()
  @callback handle_change_array?() :: boolean()

  @optional_callbacks [
    init: 1,
    storage_type: 0,
    cast_stored_array: 2,
    generator: 1,
    cast_input_array: 2,
    dump_to_native_array: 2,
    handle_change_array: 3,
    prepare_change_array: 3,
    apply_constraints_array: 2,
    array_constraints: 0,
    dump_to_embedded: 2,
    dump_to_embedded_array: 2,
    include_source: 2,
    load: 4,
    merge_load: 4,
    get_rewrites: 4,
    rewrite: 3,
    operator_overloads: 0,
    evaluate_operator: 1
  ]

  @builtin_types Keyword.values(@builtin_short_names)

  @doc false
  def builtin_types do
    @builtin_types
  end

  @doc "Returns the list of available type short names"
  def short_names, do: @short_names

  @doc "Returns true if the type is an ash builtin type"
  def builtin?(type) when type in @builtin_types, do: true
  def builtin?(_), do: false

  @doc "Returns true if the type is an embedded resource"
  def embedded_type?({:array, type}) do
    embedded_type?(type)
  end

  def embedded_type?(type) do
    type = get_type(type)
    type.embedded?()
  end

  @doc "Calls the type's `describe` function with the given constraints"
  def describe(type, constraints) do
    case get_type(type) do
      {:array, type} ->
        "#{type.describe(constraints)}[]"

      type ->
        type.describe(constraints)
    end
  end

  @doc "Gets the array constraints for a type"
  def array_constraints({:array, type}) do
    [items: array_constraints(type)]
  end

  def array_constraints(type) do
    type.array_constraints()
  end

  @spec get_type(atom | module | {:array, atom | module}) ::
          atom | module | {:array, atom | module}
  @doc "Gets the type module for a given short name or module"
  def get_type({:array, value}) do
    {:array, get_type(value)}
  end

  for {short_name, value} <- @short_names do
    def get_type(unquote(short_name)), do: unquote(value)
  end

  def get_type(value) when is_atom(value) do
    value
  end

  def get_type(value) do
    value
  end

  @doc "Returns true if the type is a composite type"
  @spec composite?(
          t(),
          constraints
        ) :: Enumerable.t()
  def composite?(type, constraints) do
    type = get_type(type)
    type.composite?(constraints)
  end

  @doc "Returns the wrapped composite types"
  @spec composite_types(
          t(),
          constraints
        ) :: Enumerable.t()
  def composite_types(type, constraints) do
    type = get_type(type)
    type.composite_types(constraints)
  end

  defp item_constraints(constraints) do
    item_constraints = Keyword.get(constraints, :items) || []

    case Keyword.fetch(constraints, :__source__) do
      {:ok, source} -> Keyword.put(item_constraints, :__source__, source)
      :error -> item_constraints
    end
  end

  @doc "Returns the StreamData generator for a given type"
  @spec generator(
          module | {:array, module},
          constraints
        ) :: Enumerable.t()
  def generator(type, constraints) do
    do_generator(type, constraints)
  end

  defp do_generator({:array, type}, constraints) do
    item_constraints = item_constraints(constraints)
    generator = do_generator(type, item_constraints)

    generator =
      if constraints[:nil_items?] do
        StreamData.one_of([StreamData.constant(nil), generator])
      else
        generator
        |> StreamData.filter(fn value ->
          with {:ok, value} <- Ash.Type.cast_input(type, value, item_constraints),
               {:ok, value} <- Ash.Type.apply_constraints(type, value, item_constraints) do
            value != nil
          else
            _ -> true
          end
        end)
      end

    StreamData.list_of(generator, Keyword.take(constraints, [:max_length, :min_length]))
  end

  defp do_generator(type, constraints) do
    type = get_type(type)

    Code.ensure_compiled!(type)

    if Ash.Type.embedded_type?(type) do
      action =
        constraints[:create_action] || Ash.Resource.Info.primary_action!(type, :create).name

      Ash.Generator.action_input(type, action)
    else
      type.generator(constraints)
    end
  end

  @doc """
  Process the old casted values alongside the new casted values.

  This is leveraged by embedded types to know if something is being updated
  or destroyed. This is not called on creates.
  """
  def handle_change({:array, {:array, _type}}, _, new_value, _) do
    {:ok, new_value}
  end

  def handle_change({:array, type}, old_value, new_value, constraints) do
    type = get_type(type)
    type.handle_change_array(old_value, new_value, item_constraints(constraints))
  end

  def handle_change(type, old_value, new_value, constraints) do
    type = get_type(type)
    type.handle_change(old_value, new_value, constraints)
  end

  @doc """
  Process the old casted values alongside the new *un*casted values.

  This is leveraged by embedded types to know if something is being updated
  or destroyed. This is not called on creates.
  """
  # Callback does not currently support this
  def prepare_change({:array, {:array, _type}}, _, new_value, _) do
    {:ok, new_value}
  end

  def prepare_change({:array, type}, old_value, new_value, constraints) do
    type = get_type(type)
    type.prepare_change_array(old_value, new_value, item_constraints(constraints))
  end

  def prepare_change(type, old_value, new_value, constraints) do
    type = get_type(type)
    type.prepare_change(old_value, new_value, constraints)
  end

  @doc """
  Initializes the constraints according to the underlying type
  """
  @spec init(t(), constraints) :: {:ok, constraints} | {:error, Ash.Error.t()}
  def init({:array, type}, constraints) do
    item_constraints = item_constraints(constraints)

    case init(type, item_constraints) do
      {:ok, new_item_constraints} ->
        {:ok, Keyword.put(constraints, :items, new_item_constraints)}

      {:error, error} ->
        {:error, error}
    end
  end

  def init(type, constraints) do
    type = get_type(type)
    type.init(constraints)
  end

  @doc """
  Returns the *underlying* storage type (the underlying type of the *ecto type* of the *ash type*)
  """
  @spec storage_type(t()) :: Ecto.Type.t()
  def storage_type(type, constraints \\ [])
  def storage_type({:array, type}, constraints), do: {:array, storage_type(type, constraints)}
  def storage_type(type, constraints), do: type.storage_type(constraints)

  @doc """
  Returns the ecto compatible type for an Ash.Type.

  If you `use Ash.Type`, this is created for you. For builtin types
  this may return a corresponding ecto builtin type (atom)
  """
  @spec ecto_type(t) :: Ecto.Type.t()
  def ecto_type({:array, type}), do: {:array, ecto_type(type)}

  for {name, mod} <- @short_names do
    def ecto_type(unquote(name)), do: ecto_type(unquote(mod))
  end

  def ecto_type(type) do
    if Ash.Resource.Info.resource?(type) do
      Module.concat(type, EctoType)
    else
      type.ecto_type()
    end
  end

  @spec ash_type?(term) :: boolean
  @doc "Returns true if the value is a builtin type or adopts the `Ash.Type` behaviour"
  def ash_type?({:array, value}), do: ash_type?(value)

  def ash_type?(module) when is_atom(module) do
    ash_type_module?(module)
  end

  def ash_type?(_), do: false

  @doc """
  Casts input (e.g. unknown) data to an instance of the type, or errors

  Maps to `Ecto.Type.cast/2`
  """
  @spec cast_input(t(), term, constraints | nil) :: {:ok, term} | {:error, Keyword.t()} | :error
  def cast_input(type, term, constraints \\ nil)

  def cast_input({:array, _type}, term, _)
      when not (is_list(term) or is_map(term) or is_nil(term)) do
    {:error, "is invalid"}
  end

  def cast_input(type, term, nil) do
    with {:ok, constraints} <- Spark.Options.validate([], Ash.Type.constraints(type)),
         {:ok, constraints} <- Ash.Type.init(type, constraints) do
      cast_input(type, term, constraints)
    end
  end

  def cast_input({:array, {:array, type}}, term, constraints) do
    cond do
      is_nil(term) ->
        {:ok, nil}

      empty?(term, constraints) ->
        {:ok, []}

      is_list(term) ->
        map_while_ok(term, &cast_input({:array, type}, &1, item_constraints(constraints)))
    end
  end

  def cast_input({:array, type}, term, constraints) do
    type = get_type(type)

    cond do
      empty?(term, constraints) ->
        {:ok, []}

      is_nil(term) ->
        {:ok, nil}

      true ->
        term =
          if is_map(term) and not is_struct(term) do
            term
            |> Enum.sort_by(&elem(&1, 1))
            |> Enum.map(&elem(&1, 0))
          else
            term
          end

        type.cast_input_array(term, item_constraints(constraints))
    end
  end

  def cast_input(_, nil, _), do: {:ok, nil}

  def cast_input(type, %type{__metadata__: _} = value, _), do: {:ok, value}

  def cast_input(type, term, constraints) do
    type = get_type(type)

    case type.cast_input(term, constraints) do
      {:ok, value} ->
        {:ok, value}

      :error ->
        case term do
          "" ->
            cast_input(type, nil, constraints)

          _ ->
            {:error, "is invalid"}
        end

      {:error, other} ->
        case term do
          "" ->
            cast_input(type, nil, constraints)

          _ ->
            {:error, other}
        end
    end
  end

  defp empty?(value, constraints) do
    value in List.wrap(constraints[:empty_values])
  end

  defp map_while_ok(term, func) do
    Enum.reduce_while(term, {:ok, []}, fn item, {:ok, acc} ->
      case func.(item) do
        {:ok, result} -> {:cont, {:ok, [result | acc]}}
        other -> {:halt, other}
      end
    end)
    |> case do
      {:ok, result} -> {:ok, Enum.reverse(result)}
      other -> other
    end
  end

  @doc """
  Casts a value from the data store to an instance of the type, or errors

  Maps to `Ecto.Type.load/2`
  """
  @spec cast_stored(t(), term, constraints | nil) :: {:ok, term} | {:error, keyword()} | :error
  def cast_stored(type, term, constraints \\ [])

  def cast_stored({:array, {:array, type}}, term, constraints) do
    if is_nil(term) do
      {:ok, nil}
    else
      map_while_ok(term, &cast_stored({:array, type}, &1, item_constraints(constraints)))
    end
  end

  def cast_stored({:array, type}, term, constraints) do
    type = get_type(type)
    type.cast_stored_array(term, item_constraints(constraints))
  end

  def cast_stored(type, term, constraints) do
    type = get_type(type)

    type.cast_stored(term, constraints)
  end

  @doc """
  Confirms if a casted value matches the provided constraints.
  """
  @spec apply_constraints(t(), term, constraints()) :: {:ok, term} | {:error, String.t()}
  def apply_constraints({:array, {:array, type}}, term, constraints) do
    type = get_type(type)
    map_while_ok(term, &apply_constraints({:array, type}, &1, item_constraints(constraints)))
  end

  def apply_constraints({:array, type}, term, constraints) when is_list(term) do
    type = get_type(type)

    list_constraint_errors = list_constraint_errors(term, constraints)
    item_constraints = item_constraints(constraints)

    case list_constraint_errors do
      [] ->
        nil_items? = Keyword.get(constraints, :nil_items?, false)
        remove_nil_items? = Keyword.get(constraints, :remove_nil_items?, false)

        term
        |> Enum.with_index()
        |> Enum.reduce({[], []}, fn {item, index}, {items, errors} ->
          if type.custom_apply_constraints_array?() do
            maybe_handle_nil_item(item, index, items, errors, nil_items?, remove_nil_items?)
          else
            case apply_constraints(type, item, item_constraints) do
              {:ok, value} ->
                maybe_handle_nil_item(value, index, items, errors, nil_items?, remove_nil_items?)

              {:error, new_errors} ->
                new_errors =
                  new_errors
                  |> List.wrap()
                  |> Ash.Helpers.flatten_preserving_keywords()
                  |> Enum.map(fn
                    string when is_binary(string) ->
                      [message: string, index: index]

                    vars ->
                      Keyword.put(vars, :index, index)
                  end)

                {[item | items], List.wrap(new_errors) ++ errors}
            end
          end
        end)
        |> case do
          {terms, []} ->
            if type.custom_apply_constraints_array?() do
              case type.apply_constraints_array(Enum.reverse(terms), constraints) do
                :ok -> {:ok, term}
                other -> other
              end
            else
              {:ok, Enum.reverse(terms)}
            end

          {_, errors} ->
            {:error, errors}
        end

      errors ->
        {:error, errors}
    end
  end

  def apply_constraints({:array, _}, nil, _), do: {:ok, nil}

  def apply_constraints({:array, _}, _, _) do
    {:error, "must be a list"}
  end

  def apply_constraints(type, term, constraints) do
    type = get_type(type)

    case type.apply_constraints(term, constraints) do
      :ok -> {:ok, term}
      other -> other
    end
  end

  defp maybe_handle_nil_item(item, index, rest, errors, nil_items?, remove_nil_items?) do
    if is_nil(item) && not nil_items? do
      if remove_nil_items? do
        {rest, errors}
      else
        {[item | rest], [[message: "no nil values", index: index] | errors]}
      end
    else
      {[item | rest], errors}
    end
  end

  @doc false
  def list_constraint_errors(term, constraints) do
    length =
      if Keyword.has_key?(constraints, :max_length) ||
           Keyword.has_key?(constraints, :min_length) do
        length(term)
      else
        0
      end

    constraints
    |> Enum.reduce([], fn
      {:min_length, min_length}, errors ->
        if length < min_length do
          [message: "must have %{min} or more items", min: min_length]
        else
          errors
        end

      {:max_length, max_length}, errors ->
        if length > max_length do
          [message: "must have %{max} or fewer items", max: max_length]
        else
          errors
        end

      _, errors ->
        errors
    end)
  end

  @doc "Returns the constraint schema for a type"
  @spec constraints(t()) :: constraints()
  def constraints({:array, _type}) do
    @array_constraints
  end

  def constraints(type) do
    type = get_type(type)
    type.constraints()
  end

  @doc "Returns `true` if the type should be cast in underlying queries"
  def cast_in_query?(type, constraints \\ [])

  def cast_in_query?({:array, type}, constraints) do
    cast_in_query?(type, item_constraints(constraints))
  end

  def cast_in_query?(type, constraints) do
    type = get_type(type)

    type.cast_in_query?(constraints)
  end

  @doc """
  Casts a value from the Elixir type to a value that the data store can persist

  Maps to `Ecto.Type.dump/2`
  """
  @spec dump_to_native(t(), term, constraints | nil) ::
          {:ok, term} | {:error, keyword()} | :error
  def dump_to_native(type, term, constraints \\ [])

  def dump_to_native({:array, {:array, type}}, term, constraints) do
    map_while_ok(term, &dump_to_native({:array, type}, &1, item_constraints(constraints)))
  end

  def dump_to_native({:array, type}, term, constraints) do
    type = get_type(type)
    type.dump_to_native_array(term, item_constraints(constraints))
  end

  def dump_to_native(type, term, constraints) do
    type = get_type(type)
    type.dump_to_native(term, constraints)
  end

  @spec cast_atomic(t(), term, constraints()) ::
          {:atomic, Ash.Expr.t()} | {:error, Ash.Error.t()} | {:not_atomic, String.t()}
  def cast_atomic({:array, {:array, _}}, _term, _constraints),
    do: {:not_atomic, "cannot currently atomically update doubly nested arrays"}

  def cast_atomic({:array, type}, term, constraints) do
    type = get_type(type)

    with {:ok, value} <- maybe_cast_input({:array, type}, term, constraints) do
      type.cast_atomic_array(value, item_constraints(constraints))
    end
  end

  def cast_atomic(type, term, constraints) do
    type = get_type(type)

    with {:ok, value} <-
           maybe_cast_input(type, term, constraints) do
      type.cast_atomic(value, constraints)
    end
  end

  defp maybe_cast_input(type, term, constraints) do
    if Ash.Expr.expr?(term) do
      {:ok, term}
    else
      Ash.Type.cast_input(type, term, constraints)
    end
  end

  @doc """
  Casts a value from the Elixir type to a value that can be embedded in another data structure.

  Embedded resources expect to be stored in JSON, so this allows things like UUIDs to be stored
  as strings in embedded resources instead of binary.
  """
  @spec dump_to_embedded(t(), term, constraints | nil) ::
          {:ok, term} | {:error, keyword()} | :error
  def dump_to_embedded(type, term, constraints \\ [])

  def dump_to_embedded({:array, {:array, type}}, term, constraints) do
    map_while_ok(term, &dump_to_embedded({:array, type}, &1, item_constraints(constraints)))
  end

  def dump_to_embedded({:array, type}, term, constraints) do
    type = Ash.Type.get_type(type)

    type.dump_to_embedded_array(term, item_constraints(constraints))
  end

  def dump_to_embedded(type, term, constraints) do
    type = get_type(type)

    type.dump_to_embedded(term, constraints)
  end

  @doc """
  Determines if two values of a given type are equal.

  Maps to `Ecto.Type.equal?/3`
  """
  @spec equal?(t(), term, term) :: boolean
  def equal?({:array, type}, [nil | xs], [nil | ys]), do: equal?({:array, type}, xs, ys)

  def equal?({:array, type}, [x | xs], [y | ys]),
    do: equal?(type, x, y) && equal?({:array, type}, xs, ys)

  def equal?({:array, _}, [], []), do: true
  def equal?({:array, _}, _, _), do: false

  def equal?(type, left, right) do
    type.equal?(left, right)
  end

  @spec include_source(
          t(),
          Ash.Changeset.t() | Ash.Query.t() | Ash.ActionInput.t(),
          constraints()
        ) :: constraints()
  def include_source({:array, type}, changeset_or_query, constraints) do
    Keyword.put(
      constraints,
      :items,
      include_source(type, changeset_or_query, constraints[:items] || [])
    )
  end

  def include_source(type, changeset_or_query, constraints) do
    type = get_type(type)
    type.include_source(constraints, changeset_or_query)
  end

  @spec merge_load(
          type :: Ash.Type.t(),
          left :: term(),
          right :: term(),
          constraints :: Keyword.t(),
          context :: merge_load_context() | nil
        ) ::
          {:ok, list(term)} | :error | {:error, Ash.Error.t()}
  def merge_load({:array, type}, left, right, constraints, context) do
    merge_load(type, left, right, constraints[:items] || [], context)
  end

  def merge_load(
        type,
        left,
        right,
        constraints,
        context
      ) do
    type = Ash.Type.get_type(type)
    type.merge_load(left, right, constraints, context)
  end

  @spec loaded?(
          type :: Ash.Type.t(),
          value_or_values :: term,
          path_to_load :: list(atom),
          constraints :: Keyword.t(),
          opts :: Keyword.t()
        ) :: boolean
  def loaded?(type, values, load, constraints, opts \\ [])

  def loaded?({:array, type}, values, loads, constraints, opts) do
    loaded?(type, values, loads, constraints, opts)
  end

  def loaded?(type, values, loads, constraints, opts) when is_list(values) do
    case Keyword.get(opts, :lists, :all) do
      :all ->
        Enum.all?(values, &loaded?(type, &1, loads, constraints, opts))

      :any ->
        Enum.any?(values, &loaded?(type, &1, loads, constraints, opts))
    end
  end

  def loaded?(type, value, load_path, constraints, opts) do
    type = get_type(type)
    type.loaded?(value, load_path, constraints, opts)
  end

  @spec load(
          type :: Ash.Type.t(),
          values :: list(term),
          load :: Keyword.t(),
          constraints :: Keyword.t(),
          context :: load_context()
        ) ::
          {:ok, list(term)} | {:error, Ash.Error.t()}
  def load(_, [], _, _, _), do: {:ok, []}
  def load(_, nil, _, _, _), do: {:ok, nil}
  def load(_, %Ash.ForbiddenField{} = value, _, _, _), do: {:ok, value}

  def load({:array, type}, values, loads, constraints, context) do
    load(type, values, loads, item_constraints(constraints), context)
  end

  def load(
        type,
        values,
        loads,
        constraints,
        context
      ) do
    splicing_nil_values(values, fn values ->
      type = get_type(type)

      if can_load?(type, constraints) do
        type.load(values, loads, constraints, context)
      else
        {:error, Ash.Error.Query.InvalidLoad.exception(load: loads)}
      end
    end)
  end

  @doc """
  Gets the load rewrites for a given type, load, calculation and path.

  This is used for defining types that support a nested load statement.
  See the embedded type and union type implementations for examples of how
  to use this.
  """
  def get_rewrites({:array, type}, merged_load, calculation, path, constraints) do
    get_rewrites(type, merged_load, calculation, path, constraints[:items] || [])
  end

  def get_rewrites(type, merged_load, calculation, path, constraints) do
    type = get_type(type)
    type.get_rewrites(merged_load, calculation, path, constraints)
  end

  @doc """
  Applies rewrites to a given value.

  This is used for defining types that support a nested load statement.
  See the embedded type and union type implementations for examples of how
  to use this.
  """
  def rewrite(_type, nil, _rewrites, _constraints), do: nil
  def rewrite(_type, [], _rewrites, _constraints), do: []

  def rewrite({:array, type}, value, rewrites, constraints) when is_list(value) do
    item_constraints = constraints[:items] || []

    Enum.map(value, fn value ->
      rewrite(type, value, rewrites, item_constraints)
    end)
  end

  def rewrite(type, item, rewrites, constraints) when not is_list(item) do
    type = get_type(type)
    type.rewrite(item, rewrites, constraints)
  end

  @doc false
  def splicing_nil_values(values, callback) when is_list(values) do
    values
    |> Stream.flat_map(fn value ->
      if is_list(value) do
        value
      else
        [value]
      end
    end)
    |> Stream.with_index()
    |> Enum.reduce({[], []}, fn
      {nil, index}, {acc, nil_indices} ->
        {acc, [index | nil_indices]}

      {value, _index}, {acc, nil_indices} ->
        {[value | acc], nil_indices}
    end)
    |> then(fn {list, nil_indices} ->
      case callback.(list) do
        {:ok, new_list} ->
          nil_indices = Enum.reverse(nil_indices)
          new_list = Enum.reverse(new_list)

          {:ok, Enum.reduce(nil_indices, new_list, &List.insert_at(&2, &1, nil))}

        {:error, error} ->
          {:error, error}
      end
    end)
  end

  def splicing_nil_values(value, callback), do: callback.(value)

  @doc "Returns true if the type supports nested loads"
  @spec can_load?(t(), Keyword.t()) :: boolean
  def can_load?(type, constraints \\ [])
  def can_load?({:array, type}, constraints), do: can_load?(type, item_constraints(constraints))

  def can_load?(type, constraints) do
    type = get_type(type)
    type.can_load?(constraints)
  end

  @doc "Prepares a given array of values for an attribute change. Runs before casting."
  @spec prepare_change_array?(t()) :: boolean
  def prepare_change_array?({:array, type}),
    do: prepare_change_array?(type)

  def prepare_change_array?(type) do
    type = get_type(type)
    type.prepare_change_array?()
  end

  @doc "Handles the change of a given array of values for an attribute change. Runs after casting."
  @spec handle_change_array?(t()) :: boolean
  def handle_change_array?({:array, type}),
    do: handle_change_array?(type)

  def handle_change_array?(type) do
    type = get_type(type)
    type.handle_change_array?()
  end

  @doc """
  Determines if a type can be compared using the `==` operator.
  """
  @spec simple_equality?(t()) :: boolean
  def simple_equality?({:array, type}), do: simple_equality?(type)

  def simple_equality?(type) do
    type = get_type(type)

    type.simple_equality?()
  end

  defmacro __using__(opts) do
    quote location: :keep, generated: true do
      @behaviour Ash.Type
      @before_compile Ash.Type

      parent = __MODULE__

      @doc false
      def ash_type?, do: true

      defmodule EctoType do
        @moduledoc false
        @parent parent
        @compile {:no_warn_undefined, @parent}
        use Ecto.ParameterizedType

        @impl true
        def init(opts) do
          constraints = @parent.constraints()

          {:ok, opts} =
            opts
            |> Keyword.take(Keyword.keys(constraints))
            |> @parent.init()

          opts
        end

        @impl true
        def type(constraints) do
          @parent.storage_type(constraints)
        end

        @impl true
        def cast(term, params) do
          @parent.cast_input(term, params)
        end

        @impl true
        def load(term, _, params) do
          parent = @parent

          case parent.cast_stored(term, params) do
            {:ok, value} ->
              {:ok, value}

            _ ->
              :error
          end
        end

        @impl true
        def dump(term, _dumper, params) do
          parent = @parent

          case parent.dump_to_native(term, params) do
            {:ok, value} ->
              {:ok, value}

            _ ->
              :error
          end
        end

        @impl true
        def equal?(left, right, _params) do
          @parent.equal?(left, right)
        end

        @impl true
        def embed_as(_, _), do: :self
      end

      @impl true
      def ecto_type, do: EctoType

      @impl true
      def constraints, do: []

      @impl true
      def describe([]), do: String.trim_leading(inspect(__MODULE__), "Ash.Type.")

      def describe(constraints) do
        "#{String.trim_leading(inspect(__MODULE__), "Ash.Type.")} | #{inspect(constraints)}"
      end

      @impl true
      def apply_constraints(value, _), do: {:ok, value}

      @impl true
      def cast_in_query?(_), do: true

      @impl true
      def composite?(_constraints), do: false

      @impl true
      def composite_types(_constraints), do: []

      @impl true
      def handle_change(_old_value, new_value, _constraints), do: {:ok, new_value}

      @impl true
      def prepare_change(_old_value, new_value, _constraints), do: {:ok, new_value}

      @impl true
      def include_source(constraints, _), do: constraints

      @impl true
      def array_constraints do
        unquote(@array_constraints)
      end

      @impl true
      def merge_load(_, _, _, _), do: :error

      @impl true
      def embedded? do
        unquote(opts[:embedded?] || false)
      end

      @impl true
      def init(constraints), do: {:ok, constraints}

      @impl true
      def dump_to_embedded(value, constraints) do
        dump_to_native(value, constraints)
      end

      @impl true
      def loaded?(_, _, _, _), do: false

      @impl true
      def cast_input_array(nil, _), do: {:ok, nil}

      def cast_input_array(term, single_constraints) do
        term
        |> Stream.with_index()
        |> Enum.reduce_while({:ok, []}, fn {item, index}, {:ok, casted} ->
          case Ash.Type.cast_input(__MODULE__, item, single_constraints) do
            :error ->
              {:halt, {:error, message: "invalid value at %{index}", index: index, path: [index]}}

            {:error, keyword} ->
              errors =
                keyword
                |> List.wrap()
                |> Ash.Helpers.flatten_preserving_keywords()
                |> Enum.map(fn
                  message when is_binary(message) ->
                    [message: message, index: index, path: [index]]

                  error when is_exception(error) ->
                    error
                    |> Ash.Error.to_ash_error()
                    |> Ash.Error.set_path([index])

                  keyword ->
                    keyword
                    |> Keyword.put(:index, index)
                    |> Keyword.update(:path, [index], &[index | &1])
                end)

              {:halt, {:error, errors}}

            {:ok, value} ->
              {:cont, {:ok, [value | casted]}}
          end
        end)
        |> case do
          {:ok, result} ->
            {:ok, Enum.reverse(result)}

          {:error, error} ->
            {:error, error}
        end
      end

      @impl true
      def cast_stored_array(term, single_constraints) do
        if is_nil(term) do
          {:ok, nil}
        else
          term
          |> Enum.with_index()
          |> Enum.reverse()
          |> Enum.reduce_while({:ok, []}, fn {item, index}, {:ok, casted} ->
            case Ash.Type.cast_stored(__MODULE__, item, single_constraints) do
              :error ->
                {:halt, {:error, index: index}}

              {:error, keyword} ->
                errors =
                  keyword
                  |> List.wrap()
                  |> Ash.Helpers.flatten_preserving_keywords()
                  |> Enum.map(fn
                    string when is_binary(string) ->
                      [message: string, index: index]

                    vars ->
                      Keyword.put(vars, :index, index)
                  end)

                {:halt, {:error, errors}}

              {:ok, value} ->
                {:cont, {:ok, [value | casted]}}
            end
          end)
        end
      end

      @impl true
      def dump_to_native_array(term, single_constraints) do
        if is_nil(term) do
          {:ok, nil}
        else
          term
          |> Enum.reverse()
          |> Enum.reduce_while({:ok, []}, fn item, {:ok, dumped} ->
            case Ash.Type.dump_to_native(__MODULE__, item, single_constraints) do
              :error ->
                {:halt, :error}

              {:ok, value} ->
                {:cont, {:ok, [value | dumped]}}
            end
          end)
        end
      end

      @impl true
      def dump_to_embedded_array(term, single_constraints) do
        if is_nil(term) do
          {:ok, nil}
        else
          term
          |> Enum.reverse()
          |> Enum.reduce_while({:ok, []}, fn item, {:ok, dumped} ->
            case Ash.Type.dump_to_embedded(__MODULE__, item, single_constraints) do
              :error ->
                {:halt, :error}

              {:ok, value} ->
                {:cont, {:ok, [value | dumped]}}
            end
          end)
        end
      end

      @impl true
      def cast_atomic(_new_value, _constraints) do
        {:not_atomic, "Type `#{inspect(__MODULE__)}` does not support atomic updates"}
      end

      @impl true
      def cast_atomic_array(new_value, constraints) do
        new_value
        |> Enum.reduce_while({:atomic, []}, fn val, {:atomic, vals} ->
          case cast_atomic(val, constraints) do
            {:atomic, atomic} ->
              {:cont, {:atomic, [atomic | vals]}}

            {:not_atomic, reason} ->
              {:halt, {:not_atomic, reason}}
          end
        end)
        |> case do
          {:atomic, vals} -> {:atomic, Enum.reverse(vals)}
          {:not_atomic, reason} -> {:not_atomic, reason}
        end
      end

      @impl true
      def generator(constraints) do
        raise "generator/1 unimplemented for #{inspect(__MODULE__)}"
      end

      defoverridable constraints: 0,
                     init: 1,
                     include_source: 2,
                     describe: 1,
                     generator: 1,
                     cast_atomic: 2,
                     cast_atomic_array: 2,
                     cast_input_array: 2,
                     dump_to_native_array: 2,
                     dump_to_embedded: 2,
                     dump_to_embedded_array: 2,
                     embedded?: 0,
                     ecto_type: 0,
                     merge_load: 4,
                     array_constraints: 0,
                     apply_constraints: 2,
                     cast_stored_array: 2,
                     handle_change: 3,
                     loaded?: 4,
                     composite?: 1,
                     composite_types: 1,
                     prepare_change: 3,
                     cast_in_query?: 1
    end
  end

  defp ash_type_module?(module) do
    module.ash_type?()
  rescue
    _ -> false
  end

  @doc """
  Determine types for a given function or operator.
  """
  def determine_types(types, values) do
    Enum.map(types, fn types ->
      case types do
        :same ->
          types =
            for _ <- values do
              :same
            end

          closest_fitting_type(types, values)

        :any ->
          for _ <- values do
            :any
          end

        types ->
          closest_fitting_type(types, values)
      end
    end)
    |> Enum.filter(fn types ->
      Enum.all?(types, &(vagueness(&1) == 0))
    end)
    |> Enum.map(fn
      :any ->
        nil

      {:array, :any} ->
        nil

      type ->
        type
    end)
    |> Enum.filter(& &1)
  end

  defp closest_fitting_type(types, values) do
    types_with_values = Enum.zip(types, values)

    types_with_values
    |> fill_in_known_types()
    |> clarify_types()
  end

  defp clarify_types(types) do
    basis =
      types
      |> Enum.map(&elem(&1, 0))
      |> Enum.min_by(&vagueness(&1))

    Enum.map(types, fn {type, _value} ->
      replace_same(type, basis)
    end)
  end

  defp replace_same({:array, type}, basis) do
    {:array, replace_same(type, basis)}
  end

  defp replace_same(:same, :same) do
    :any
  end

  defp replace_same(:same, {:array, :same}) do
    {:array, :any}
  end

  defp replace_same(:same, basis) do
    basis
  end

  defp replace_same(other, _basis) do
    other
  end

  defp fill_in_known_types(types) do
    Enum.map(types, &fill_in_known_type/1)
  end

  defp fill_in_known_type({vague_type, %Ash.Query.Ref{attribute: %{type: type}}} = ref)
       when vague_type in [:any, :same] do
    if Ash.Type.ash_type?(type) do
      {type || :any, ref}
    else
      {type, ref}
    end
  end

  defp fill_in_known_type(
         {{:array, type}, %Ash.Query.Ref{attribute: %{type: {:array, type}} = attribute} = ref}
       ) do
    {:array, fill_in_known_type({type, %{ref | attribute: %{attribute | type: type}}})}
  end

  defp fill_in_known_type({type, value}), do: {array_to_in(type), value}

  defp array_to_in({:array, v}), do: {:array, array_to_in(v)}

  defp array_to_in(v), do: v

  defp vagueness({:array, type}), do: vagueness(type)
  defp vagueness(:same), do: 2
  defp vagueness(:any), do: 1
  defp vagueness(_), do: 0

  @doc false
  def set_type_transformation(%{type: original_type, constraints: constraints} = thing) do
    type = get_type(original_type)

    ash_type? = Ash.Type.ash_type?(type)

    unless ash_type? do
      raise """
      #{inspect(original_type)} is not a valid type.

      Valid types include any custom types, or the following short codes (alongside the types they map to):

      #{Enum.map_join(@short_names, "\n", fn {name, type} -> "  #{inspect(name)} -> #{inspect(type)}" end)}

      """
    end

    with {:ok, constraints} <- validate_constraints(type, constraints),
         {:ok, constraints} <- Ash.Type.init(type, constraints),
         {:ok, thing} <- set_default(thing, type, constraints),
         {:ok, thing} <- set_update_default(thing, type, constraints) do
      {:ok, %{thing | type: type, constraints: constraints}}
    end
  end

  defp set_default(%{default: {_m, _f, _a}} = thing, _type, _constraints), do: {:ok, thing}

  defp set_default(%{default: default} = thing, type, constraints)
       when not is_nil(default) and not is_function(default) do
    case Ash.Type.cast_input(type, default, constraints) do
      {:ok, value} ->
        {:ok, %{thing | default: value}}

      :error ->
        {:error, "Could not cast #{inspect(default)} to #{inspect(type)}"}

      {:error, error} ->
        {:error, "Could not cast #{inspect(default)} to #{inspect(type)}: #{inspect(error)}"}
    end
  end

  defp set_default(thing, _type, _constraints), do: {:ok, thing}

  defp set_update_default(%{update_default: {_m, _f, _a}} = thing, _type, _constraints),
    do: {:ok, thing}

  defp set_update_default(%{update_default: update_default} = thing, type, constraints)
       when not is_nil(update_default) and not is_function(update_default) do
    case Ash.Type.cast_input(type, update_default, constraints) do
      {:ok, value} ->
        {:ok, %{thing | update_default: value}}

      :error ->
        {:error, "Could not cast #{inspect(update_default)} to #{inspect(type)}"}

      {:error, error} ->
        {:error,
         "Could not cast #{inspect(update_default)} to #{inspect(type)}: #{inspect(error)}"}
    end
  end

  defp set_update_default(thing, _type, _constraints), do: {:ok, thing}

  @doc false
  def validate_constraints(type, constraints) do
    case type do
      {:array, type} ->
        array_constraints = array_constraints(type)

        with {:ok, new_constraints} <-
               Spark.Options.validate(
                 Keyword.delete(constraints || [], :items),
                 Keyword.delete(array_constraints, :items)
               ),
             {:ok, item_constraints} <- validate_constraints(type, item_constraints(constraints)) do
          {:ok, Keyword.put(new_constraints, :items, item_constraints)}
        end

      type ->
        schema = constraints(type)

        case Spark.Options.validate(constraints, schema) do
          {:ok, constraints} ->
            validate_none_reserved(constraints, type)

          {:error, error} ->
            {:error, error}
        end
    end
  end

  @reserved ~w(default source autogenerate read_after_writes virtual primary_key load_in_query redact)a

  defp validate_none_reserved(constraints, type) do
    case Enum.find(@reserved, &Keyword.has_key?(constraints, &1)) do
      nil ->
        {:ok, constraints}

      key ->
        {:error,
         "Invalid constraint key #{key} in type #{inspect(type)}. This name is reserved due to the underlying ecto implementation."}
    end
  end

  # Credit to @immutable from elixir discord for the idea
  defmacro __before_compile__(_env) do
    quote generated: true do
      if Module.defines?(__MODULE__, {:equal?, 2}, :def) do
        unless Module.defines?(__MODULE__, {:simple_equality, 0}, :def) do
          @impl true
          def simple_equality?, do: false
        end
      else
        unless Module.defines?(__MODULE__, {:simple_equality, 0}, :def) do
          @impl true
          def simple_equality?, do: true
        end

        @impl true
        def equal?(left, right), do: left == right
      end

      if Module.defines?(__MODULE__, {:handle_change_array, 3}, :def) do
        @impl true
        def handle_change_array?, do: true
      else
        @impl true
        def handle_change_array(_old_value, new_value, _constraints) do
          {:ok, new_value}
        end

        @impl true
        def handle_change_array?, do: false
      end

      if Module.defines?(__MODULE__, {:prepare_change_array, 3}, :def) do
        @impl true
        def prepare_change_array?, do: true
      else
        @impl true
        def prepare_change_array(_old_value, new_value, _constraints) do
          {:ok, new_value}
        end

        @impl true
        def prepare_change_array?, do: false
      end

      cond do
        Module.defines?(__MODULE__, {:storage_type, 0}) &&
            Module.defines?(__MODULE__, {:storage_type, 1}) ->
          raise "Must only define storage_type/0 or storage_type/1 but not both"

        Module.defines?(__MODULE__, {:storage_type, 0}) ->
          @impl Ash.Type
          def storage_type(_constraints), do: storage_type()

        true ->
          :ok
      end

      @impl Ash.Type
      if Module.defines?(__MODULE__, {:apply_constraints_array, 2}, :def) do
        def custom_apply_constraints_array?, do: true
      else
        def custom_apply_constraints_array?, do: false
      end

      unless Module.defines?(__MODULE__, {:can_load?, 1}, :def) do
        @impl Ash.Type
        if Module.defines?(__MODULE__, {:load, 4}, :def) do
          def can_load?(_), do: true
        else
          def can_load?(_), do: false
        end
      end
    end
  end
end
