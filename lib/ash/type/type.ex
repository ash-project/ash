defmodule Ash.Type do
  @array_constraints [
    min_length: [
      type: :non_neg_integer,
      doc: "A minimum length for the items"
    ],
    items: [
      type: :any,
      doc: "A schema for individual items"
    ],
    max_length: [
      type: :non_neg_integer,
      doc: "A maximum length for the items"
    ],
    nil_items?: [
      type: :boolean,
      doc: "Whether or not the list can contain nil items",
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
  Describes how to convert data to `Ecto.Type` and eventually into the database.

  This behaviour is a superset of the `Ecto.Type` behaviour, that also contains
  API level information, like what kinds of filters are allowed.

  ## Built in types

  #{Enum.map_join(@builtin_short_names, fn {key, module} -> "* `#{inspect(key)}` - `#{inspect(module)}`\n" end)}

  ## Composite Types

  Currently, the only composite type supported is a list type, specified via:
  `{:array, Type}`. The constraints available are:

  #{Spark.OptionsHelpers.docs(@doc_array_constraints)}

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
  """

  @type constraints :: Keyword.t()
  @type constraint_error :: String.t() | {String.t(), Keyword.t()}
  @type t :: atom | {:array, atom}
  @type error :: :error | {:error, String.t() | Keyword.t()}

  @type load_context :: %{
          api: Ash.Api.t(),
          actor: term() | nil,
          tenant: String.t() | nil,
          tracer: list(Ash.Tracer.t()) | Ash.Tracer.t() | nil,
          authorize?: boolean | nil
        }

  @callback storage_type() :: Ecto.Type.t()
  @callback storage_type(constraints) :: Ecto.Type.t()
  @callback include_source(constraints, Ash.Changeset.t()) :: constraints
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
  @callback load(
              values :: list(term),
              load :: Keyword.t(),
              constraints :: Keyword.t(),
              context :: load_context()
            ) ::
              {:ok, list(term)} | {:error, Ash.Error.t()}

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
    load: 4
  ]

  @builtin_types Keyword.values(@builtin_short_names)

  @doc false
  def builtin_types do
    @builtin_types
  end

  def short_names, do: @short_names

  def builtin?(type) when type in @builtin_types, do: true
  def builtin?(_), do: false

  def embedded_type?({:array, type}) do
    embedded_type?(type)
  end

  def embedded_type?(type) do
    type = get_type(type)
    type.embedded?()
  end

  def describe(type, constraints) do
    case get_type(type) do
      {:array, type} ->
        type.describe(constraints)

      type ->
        type.describe(constraints)
    end
  end

  def array_constraints({:array, type}) do
    [items: array_constraints(type)]
  end

  def array_constraints(type) do
    type.array_constraints()
  end

  @spec get_type(atom | module | {:array, atom | module}) ::
          atom | module | {:array, atom | module}
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

  @spec generator(
          module | {:array, module},
          constraints
        ) :: Enumerable.t()
  def generator(type, constraints) do
    do_generator(type, constraints)
  end

  defp do_generator({:array, type}, constraints) do
    generator = do_generator(type, constraints[:items] || [])

    generator =
      if constraints[:nil_items?] do
        StreamData.one_of([StreamData.constant(nil), generator])
      else
        generator
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
      if function_exported?(type, :generator, 1) do
        type.generator(constraints)
      else
        raise "generator/1 unimplemented for #{inspect(type)}"
      end
    end
  end

  @doc """
  Process the old casted values alongside the new casted values.

  This is leveraged by embedded types to know if something is being updated
  or destroyed. This is not called on creates.
  """
  def handle_change({:array, type}, old_value, new_value, constraints) do
    if is_atom(type) && :erlang.function_exported(type, :handle_change_array, 3) do
      type.handle_change_array(old_value, new_value, constraints)
    else
      {:ok, new_value}
    end
  end

  def handle_change(type, old_value, new_value, constraints) do
    type.handle_change(old_value, new_value, constraints)
  end

  @doc """
  Process the old casted values alongside the new *un*casted values.

  This is leveraged by embedded types to know if something is being updated
  or destroyed. This is not called on creates.
  """
  def prepare_change({:array, type}, old_value, new_value, constraints) do
    if is_atom(type) && :erlang.function_exported(type, :prepare_change_array, 3) do
      type.prepare_change_array(old_value, new_value, constraints)
    else
      {:ok, new_value}
    end
  end

  def prepare_change(type, old_value, new_value, constraints) do
    type.prepare_change(old_value, new_value, constraints)
  end

  @doc """
  Initializes the constraints according to the underlying type
  """
  @spec init(t(), constraints) :: {:ok, constraints} | {:error, Ash.Error.t()}
  def init({:array, type}, constraints) do
    item_constraints = constraints[:items] || []

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

  def ash_type_option(type) do
    type = get_type(type)

    if ash_type?(type) do
      {:ok, type}
    else
      {:error, "Attribute type must be a built in type or a type module, got: #{inspect(type)}"}
    end
  end

  @spec ash_type?(term) :: boolean
  @doc "Returns true if the value is a builtin type or adopts the `Ash.Type` behaviour"
  def ash_type?({:array, value}), do: ash_type?(value)

  def ash_type?(module) when is_atom(module) do
    ash_type_module?(module) || Ash.Resource.Info.resource?(module)
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
    with {:ok, constraints} <-
           Spark.OptionsHelpers.validate([], Ash.Type.constraints(type)),
         {:ok, constraints} <- Ash.Type.init(type, constraints) do
      cast_input(type, term, constraints)
    end
  end

  def cast_input({:array, type}, term, constraints) do
    cond do
      empty?(term, constraints) ->
        {:ok, []}

      is_nil(term) ->
        {:ok, nil}

      true ->
        term =
          if is_map(term) do
            term
            |> Enum.sort_by(&elem(&1, 1))
            |> Enum.map(&elem(&1, 0))
          else
            term
          end

        if is_atom(type) && :erlang.function_exported(type, :cast_input_array, 2) do
          type.cast_input_array(term, constraints)
        else
          single_constraints = constraints[:items] || []

          term
          |> Enum.with_index()
          |> Enum.reverse()
          |> Enum.reduce_while({:ok, []}, fn {item, index}, {:ok, casted} ->
            case cast_input(type, item, single_constraints) do
              :error ->
                {:halt,
                 {:error, message: "invalid value at %{index}", index: index, path: [index]}}

              {:error, keyword} ->
                errors =
                  keyword
                  |> List.wrap()
                  |> Ash.Error.flatten_preserving_keywords()
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
        end
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

  @doc """
  Casts a value from the data store to an instance of the type, or errors

  Maps to `Ecto.Type.load/2`
  """
  @spec cast_stored(t(), term, constraints | nil) :: {:ok, term} | {:error, keyword()} | :error
  def cast_stored(type, term, constraints \\ [])

  def cast_stored({:array, type}, term, constraints) do
    if is_atom(type) && :erlang.function_exported(type, :cast_stored_array, 2) do
      type.cast_stored_array(term, constraints)
    else
      if is_nil(term) do
        {:ok, nil}
      else
        term
        |> Enum.with_index()
        |> Enum.reverse()
        |> Enum.reduce_while({:ok, []}, fn {item, index}, {:ok, casted} ->
          single_constraints = constraints[:items] || []

          case cast_stored(type, item, single_constraints) do
            :error ->
              {:halt, {:error, index: index}}

            {:error, keyword} ->
              errors =
                keyword
                |> List.wrap()
                |> Ash.Error.flatten_preserving_keywords()
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
  end

  def cast_stored(type, term, constraints) do
    type = get_type(type)

    type.cast_stored(term, constraints)
  end

  @doc """
  Confirms if a casted value matches the provided constraints.
  """
  @spec apply_constraints(t(), term, constraints()) :: {:ok, term} | {:error, String.t()}
  def apply_constraints({:array, type}, term, constraints) when is_list(term) do
    if is_atom(type) && :erlang.function_exported(type, :apply_constraints_array, 2) do
      case type.apply_constraints_array(term, constraints) do
        :ok -> {:ok, term}
        other -> other
      end
    else
      list_constraint_errors = list_constraint_errors(term, constraints)

      case list_constraint_errors do
        [] ->
          nil_items? = Keyword.get(constraints, :nil_items?, false)
          item_constraints = constraints[:items] || []

          term
          |> Enum.with_index()
          |> Enum.reduce({[], []}, fn {item, index}, {items, errors} ->
            if is_nil(item) && not nil_items? do
              {[item | items], [[message: "no nil values", index: index] | errors]}
            else
              case apply_constraints(type, item, item_constraints) do
                {:ok, value} ->
                  {[value | items], errors}

                {:error, new_errors} ->
                  new_errors =
                    new_errors
                    |> List.wrap()
                    |> Ash.Error.flatten_preserving_keywords()
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
              {:ok, Enum.reverse(terms)}

            {_, errors} ->
              {:error, errors}
          end

        errors ->
          {:error, errors}
      end
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

  @doc false
  def list_constraint_errors(term, constraints) do
    length =
      if Keyword.has_key?(constraints, :max_length) || Keyword.has_key?(constraints, :min_length) do
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

  @spec constraints(t()) :: constraints()
  def constraints({:array, _type}) do
    @array_constraints
  end

  def constraints(type) do
    type = get_type(type)
    type.constraints()
  end

  def cast_in_query?(type, constraints \\ [])

  def cast_in_query?({:array, type}, constraints) do
    cast_in_query?(type, constraints[:items] || [])
  end

  def cast_in_query?(type, constraints) do
    type = get_type(type)

    if function_exported?(type, :cast_in_query?, 0) do
      type.cast_in_query?()
    else
      type.cast_in_query?(constraints)
    end
  end

  @doc """
  Casts a value from the Elixir type to a value that the data store can persist

  Maps to `Ecto.Type.dump/2`
  """
  @spec dump_to_native(t(), term, constraints | nil) :: {:ok, term} | {:error, keyword()} | :error
  def dump_to_native(type, term, constraints \\ [])

  def dump_to_native({:array, type}, term, constraints) do
    if is_atom(type) && :erlang.function_exported(type, :dump_to_native_array, 2) do
      type.dump_to_native_array(term, constraints)
    else
      if is_nil(term) do
        {:ok, nil}
      else
        single_constraints = constraints[:items] || []

        term
        |> Enum.reverse()
        |> Enum.reduce_while({:ok, []}, fn item, {:ok, dumped} ->
          case dump_to_native(type, item, single_constraints) do
            :error ->
              {:halt, :error}

            {:ok, value} ->
              {:cont, {:ok, [value | dumped]}}
          end
        end)
      end
    end
  end

  def dump_to_native(type, term, constraints) do
    type = get_type(type)
    type.dump_to_native(term, constraints)
  end

  @doc """
  Casts a value from the Elixir type to a value that can be embedded in another data structure.

  Embedded resources expect to be stored in JSON, so this allows things like UUIDs to be stored
  as strings in embedded resources instead of binary.
  """
  @spec dump_to_embedded(t(), term, constraints | nil) ::
          {:ok, term} | {:error, keyword()} | :error
  def dump_to_embedded(type, term, constraints \\ [])

  def dump_to_embedded({:array, type}, term, constraints) do
    if is_atom(type) && :erlang.function_exported(type, :dump_to_embedded_array, 2) do
      type.dump_to_embedded_array(term, constraints)
    else
      if is_nil(term) do
        {:ok, nil}
      else
        single_constraints = constraints[:items] || []

        term
        |> Enum.reverse()
        |> Enum.reduce_while({:ok, []}, fn item, {:ok, dumped} ->
          case dump_to_embedded(type, item, single_constraints) do
            :error ->
              {:halt, :error}

            {:ok, value} ->
              {:cont, {:ok, [value | dumped]}}
          end
        end)
      end
    end
  end

  def dump_to_embedded(type, term, constraints) do
    type = get_type(type)

    if :erlang.function_exported(type, :dump_to_embedded, 2) do
      type.dump_to_embedded(term, constraints)
    else
      type.dump_to_native(term, constraints)
    end
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

  @spec include_source(t(), Ash.Changeset.t() | Ash.Query.t(), constraints()) :: constraints()
  def include_source({:array, type}, changeset_or_query, constraints) do
    include_source(type, changeset_or_query, constraints)
  end

  def include_source(type, changeset_or_query, constraints) do
    type = get_type(type)
    type.include_source(changeset_or_query, constraints)
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
    load(type, values, loads, constraints[:items] || [], context)
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

  @spec can_load?(t(), Keyword.t()) :: boolean
  def can_load?(type, constraints \\ [])
  def can_load?({:array, type}, constraints), do: can_load?(type, constraints[:items] || [])

  def can_load?(type, constraints) do
    type = get_type(type)
    type.can_load?(constraints)
  end

  @doc """
  Determines if a type can be compared using ==
  """
  @spec simple_equality?(t()) :: boolean
  def simple_equality?({:array, type}), do: simple_equality?(type)

  def simple_equality?(type) do
    type = get_type(type)

    type.simple_equality?()
  end

  # @callback equal?(term, term) :: boolean

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
      def handle_change(_old_value, new_value, _constraints), do: {:ok, new_value}

      @impl true
      def prepare_change(_old_value, new_value, _constraints), do: {:ok, new_value}

      @impl true
      def include_source(_, constraints), do: constraints

      @impl true
      def array_constraints do
        unquote(@array_constraints)
      end

      @impl true
      def embedded? do
        unquote(opts[:embedded?] || false)
      end

      @impl true
      def init(constraints), do: {:ok, constraints}

      defoverridable constraints: 0,
                     init: 1,
                     include_source: 2,
                     describe: 1,
                     embedded?: 0,
                     ecto_type: 0,
                     array_constraints: 0,
                     apply_constraints: 2,
                     handle_change: 3,
                     prepare_change: 3,
                     cast_in_query?: 1
    end
  end

  defp ash_type_module?(module) do
    module.ash_type?()
  rescue
    _ -> false
  end

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
         {:ok, constraints} <- Ash.Type.init(type, constraints) do
      {:ok, %{thing | type: type, constraints: constraints}}
    end
  end

  @doc false
  def validate_constraints(type, constraints) do
    case type do
      {:array, type} ->
        array_constraints = array_constraints(type)

        with {:ok, new_constraints} <-
               Spark.OptionsHelpers.validate(
                 Keyword.delete(constraints || [], :items),
                 Keyword.delete(array_constraints, :items)
               ),
             {:ok, item_constraints} <- validate_constraints(type, constraints[:items] || []) do
          {:ok, Keyword.put(new_constraints, :items, item_constraints)}
        end

      type ->
        schema = constraints(type)

        case Spark.OptionsHelpers.validate(constraints, schema) do
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
