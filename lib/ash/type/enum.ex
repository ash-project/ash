defmodule Ash.Type.Enum do
  @moduledoc """
  A type for abstracting enums into a single type.

  For example, your existing attribute might look like:
  ```elixir
  attribute :status, :atom, constraints: [one_of: [:open, :closed]]
  ```

  But as that starts to spread around your system, you may find that you want
  to centralize that logic. To do that, use this module to define an Ash type
  easily:

  ```elixir
  defmodule MyApp.TicketStatus do
    use Ash.Type.Enum, values: [:open, :closed]
  end
  ```

  Then, you can rewrite your original attribute as follows:

  ```elixir
  attribute :status, MyApp.TicketStatus
  ```

  Valid values are:

  * The atom itself, e.g `:open`
  * A string that matches the atom, e.g `"open"`
  * A string that matches the atom after being downcased, e.g `"OPEN"` or `"oPeN"`
  * A string that matches the stringified, downcased atom, after itself being downcased.
    This allows for enum values like `:Open`, `:SomeState` and `:Some_State`

  ## Custom input values

  If you need to accept inputs beyond those described above while still mapping them to one
  of the enum values, you can override the `match/1` callback.

  For example, if you want to map both the `:half_empty` and `:half_full` states to the same enum
  value, you could implement it as follows:

  ```elixir
  defmodule MyApp.GlassState do
    use Ash.Type.Enum, values: [:empty, :half_full, :full]

    def match(:half_empty), do: {:ok, :half_full}
    def match("half_empty"), do: {:ok, :half_full}
    def match(value), do: super(value)
  end
  ```

  In the provided example, if no additional value is matched, `super(value)` is called, invoking
  the default implementation of `match/1`. This approach is typically suitable if you only aim to
  extend default matching rather than completely reimplementing it.

  ### Caveats

  Additional input values are not exposed in derived interfaces. For example, `HALF_EMPTY` will not
  be present as a possible enum value when using `ash_graphql`.

  Moreover, only explicitly matched values are mapped to the enum value. For instance,
  `"HaLf_emPty"` would not be accepted by the code provided earlier. If case normalization is
  needed for additional values, it must be explicitly implemented.

  ## Value descriptions
  It's possible to associate a description with a value by passing a `{value, description}` tuple
  inside the values list, which becomes a keyword list:

  ```elixir
  defmodule MyApp.TicketStatus do
    use Ash.Type.Enum,
      values: [
        open: "An open ticket",
        closed: "A closed ticket"
      ]
  end
  ```

  This can be used by extensions to provide detailed descriptions of enum values.

  The description of a value can be retrieved with `description/1`:

  ```elixir
  MyApp.TicketStatus.description(:open)
  iex> "An open ticket"
  ```
  """
  @doc "The list of valid values (not all input types that match them)"
  @callback values() :: [atom | String.t()]
  @doc "The description of the value, if existing"
  @callback description(atom | String.t()) :: String.t() | nil
  @doc "true if a given term matches a value"
  @callback match?(term) :: boolean
  @doc "finds the valid value that matches a given input term"
  @callback match(term) :: {:ok, atom} | :error

  defmacro __using__(opts) do
    quote location: :keep, generated: true, bind_quoted: [opts: opts, behaviour: __MODULE__] do
      use Ash.Type

      require Ash.Expr

      @behaviour behaviour

      @values behaviour.build_values(opts[:values])

      atom_typespec =
        if Enum.any?(@values, &is_atom/1) do
          @values
          |> Enum.filter(&is_atom/1)
          |> Enum.reduce(&{:|, [], [&1, &2]})
        end

      typespec =
        if Enum.any?(@values, &(not is_atom(&1))) do
          if atom_typespec do
            {:|, [],
             [atom_typespec, {{:., [], [{:__aliases__, [alias: false], [:String]}, :t]}, [], []}]}
          else
            {{:., [], [{:__aliases__, [alias: false], [:String]}, :t]}, [], []}
          end
        else
          if atom_typespec do
            atom_typespec
          else
            {:term, [], Elixir}
          end
        end

      @type t() :: unquote(typespec)

      @description_map behaviour.build_description_map(opts[:values])

      @string_values @values |> Enum.map(&to_string/1)

      @any_not_downcase? Enum.any?(@string_values, fn value -> String.downcase(value) != value end)

      @impl behaviour
      def values, do: @values

      @impl behaviour
      def description(value) when value in @values, do: Map.get(@description_map, value)

      @impl Ash.Type
      def storage_type, do: :string

      @impl Ash.Type
      def generator(_constraints) do
        StreamData.member_of(@values)
      end

      @impl Ash.Type
      def cast_input(nil, _) do
        {:ok, nil}
      end

      def cast_input(value, _) do
        match(value)
      end

      @impl true
      def matches_type?(value, _) when value in @values, do: true
      def matches_type?(_, _), do: false

      @impl Ash.Type
      def cast_stored(nil, _), do: {:ok, nil}

      def cast_stored(value, _) do
        match(value)
      end

      @impl Ash.Type
      def dump_to_native(nil, _) do
        {:ok, nil}
      end

      def dump_to_native(value, _) do
        {:ok, to_string(value)}
      end

      @impl true
      def cast_atomic(new_value, constraints) do
        if Ash.Expr.expr?(new_value) do
          if @any_not_downcase? do
            {:atomic, new_value}
          else
            {:atomic, Ash.Expr.expr(string_downcase(^new_value))}
          end
        else
          case cast_input(new_value, constraints) do
            {:ok, value} -> {:atomic, value}
            {:error, error} -> {:error, error}
          end
        end
      end

      @impl true
      def apply_atomic_constraints(new_value, constraints) do
        if Ash.Expr.expr?(new_value) do
          if @any_not_downcase? do
            error_expr =
              Ash.Expr.expr(
                error(
                  Ash.Error.Changes.InvalidChanges,
                  message: "must be one of %{values}",
                  vars: %{values: ^Enum.join(@values, ", ")}
                )
              )

            Enum.reduce(@values, {:atomic, error_expr}, fn valid_value, {:atomic, expr} ->
              expr =
                Ash.Expr.expr(
                  if string_downcase(^new_value) == string_downcase(^valid_value) do
                    ^valid_value
                  else
                    ^expr
                  end
                )

              {:atomic, expr}
            end)
          else
            {:ok,
             Ash.Expr.expr(
               if ^new_value in ^@values do
                 ^new_value
               else
                 error(
                   Ash.Error.Changes.InvalidChanges,
                   message: "must be one of %{values}",
                   vars: %{values: ^Enum.join(@values, ", ")}
                 )
               end
             )}
          end
        else
          apply_constraints(new_value, constraints)
        end
      end

      @impl behaviour
      @spec match?(term) :: boolean
      def match?(term) do
        case match(term) do
          {:ok, _} -> true
          _ -> false
        end
      end

      @impl behaviour
      @spec match(term) :: {:ok, atom} | :error
      def match(value) when value in @values, do: {:ok, value}
      def match(value) when value in @string_values, do: {:ok, String.to_existing_atom(value)}

      def match(value) do
        value =
          value
          |> to_string()
          |> String.downcase()

        match =
          Enum.find_value(@values, fn valid_value ->
            sanitized_valid_value =
              valid_value
              |> to_string()
              |> String.downcase()

            if sanitized_valid_value == value do
              valid_value
            end
          end)

        if match do
          {:ok, match}
        else
          :error
        end
      rescue
        _ ->
          :error
      end

      defoverridable match: 1, storage_type: 0, cast_stored: 2, dump_to_native: 2
    end
  end

  @doc false
  def build_description_map(values) do
    values
    |> verify_values!()
    |> Enum.reduce(%{}, fn
      {value, description}, acc when is_binary(description) -> Map.put(acc, value, description)
      _value_with_no_description, acc -> acc
    end)
  end

  @doc false
  def build_values(values) do
    values
    |> verify_values!()
    |> Enum.map(fn
      {value, _description} -> value
      value -> value
    end)
  end

  @doc false
  def verify_values!(values) when is_list(values) do
    Enum.each(values, fn
      value when is_atom(value) or is_binary(value) ->
        :ok

      {value, nil} when is_atom(value) or is_binary(value) ->
        :ok

      {value, description} when (is_atom(value) or is_binary(value)) and is_binary(description) ->
        :ok

      other ->
        raise(
          "`values` must be a list of `atom | string` or {`atom | string`, string} tuples, got #{inspect(other)}"
        )
    end)

    values
  end

  def verify_values!(nil) do
    raise("Must provide `values` option for `use #{inspect(__MODULE__)}`")
  end

  def verify_values!(values) do
    raise("Must provide a list in `values`, got #{inspect(values)}")
  end
end
