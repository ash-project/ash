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
  """
  @doc "The list of valid values (not all input types that match them)"
  @callback values() :: [atom]
  @doc "true if a given term matches a value"
  @callback match?(term) :: boolean
  @doc "finds the valid value that matches a given input term"
  @callback match(term) :: {:ok, atom} | :error

  defmacro __using__(opts) do
    quote location: :keep, generated: true do
      use Ash.Type

      require Ash.Expr

      @behaviour unquote(__MODULE__)

      @values unquote(opts[:values]) ||
                raise("Must provide `values` option for `use #{inspect(unquote(__MODULE__))}`")
      @string_values @values |> Enum.map(&to_string/1)

      @any_not_downcase? Enum.any?(@string_values, fn value -> String.downcase(value) != value end)

      @impl unquote(__MODULE__)
      def values, do: @values

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
      def cast_atomic(new_value, _constraints) do
        if @any_not_downcase? do
          {:atomic,
           Ash.Expr.expr(
             if ^new_value in ^@values do
               string_downcase(^new_value)
             else
               error(
                 Ash.Error.Changes.InvalidChanges,
                 message: "must be one of %{values}",
                 vars: %{values: Enum.map_join(@values, ", ")}
               )
             end
           )}
        else
          error_expr =
            Ash.Expr.expr(
              error(
                Ash.Error.Changes.InvalidChanges,
                message: "must be one of %{values}",
                vars: %{values: Enum.map_join(@values, ", ")}
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
        end
      end

      @impl unquote(__MODULE__)
      @spec match?(term) :: boolean
      def match?(term) do
        case match(term) do
          {:ok, _} -> true
          _ -> false
        end
      end

      @impl unquote(__MODULE__)
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

      defoverridable storage_type: 0
    end
  end
end
