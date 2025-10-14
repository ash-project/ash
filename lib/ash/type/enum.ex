# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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

  ## Value labels and descriptions
  It's possible to associate a label and/or description for each value.

  ```elixir
  defmodule MyApp.TicketStatus do
    use Ash.Type.Enum,
      values: [
        open: "An open ticket", # <- description only,
        escalated: [description: "An escalated ticket"],
        follow_up: [label: "Follow up"],
        closed: [description: "A closed ticket", label: "Closed"]
      ]
  end
  ```

  Adding labels and descriptions can be helpful when displaying the Enum values to users.

  This can be used by extensions to provide detailed descriptions of enum values.

  The description of a value can be retrieved with `description/1`:

  ```elixir
  MyApp.TicketStatus.description(:open)
  iex> "An open ticket"
  ```

  The label of a value can be retrieved with `label/1`:

  ```elixir
  MyApp.TicketStatus.label(:closed)
  iex> "Closed"
  ```

  A default label is generated based on the value.
  ```elixir
  MyApp.TicketStatus.label(:open)
  iex> "Open"
  ```

  Both the description and label can be retrieved with `details/1`

  ```elixir
  MyApp.TicketStatus.details(:closed)
  iex> %{description: "A closed ticket", label: "Closed"}
  ```
  """
  @doc "The list of valid values (not all input types that match them)"
  @callback values() :: [atom | String.t()]
  @doc "The label of the value, if existing"
  @callback label(atom | String.t()) :: String.t() | nil
  @doc "The description of the value, if existing"
  @callback description(atom | String.t()) :: String.t() | nil
  @doc "The value detail map, if existing"
  @callback details(atom | String.t()) :: %{
              description: String.t() | nil,
              label: String.t() | nil
            }
  @doc "true if a given term matches a value"
  @callback match?(term) :: boolean
  @doc "finds the valid value that matches a given input term"
  @callback match(term) :: {:ok, atom} | :error

  defmacro __using__(opts) do
    quote location: :keep, generated: true, bind_quoted: [opts: opts] do
      use Ash.Type

      require Ash.Expr

      @behaviour Ash.Type.Enum

      @values_map Ash.Type.Enum.build_values_map(opts[:values])
      @values Ash.Type.Enum.build_values_list(opts[:values])

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

      @string_values Enum.map(@values, &to_string/1)

      @any_not_downcase? Enum.any?(@string_values, &(String.downcase(&1) != &1))

      @impl Ash.Type.Enum
      def values, do: @values

      @impl Ash.Type.Enum
      def label(value) when value in @values do
        value
        |> details()
        |> Map.get(:label)
      end

      @impl Ash.Type.Enum
      def description(value) when value in @values do
        value
        |> details()
        |> Map.get(:description)
      end

      @impl Ash.Type.Enum
      def details(value) when value in @values do
        Map.get(@values_map, value)
      end

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

      @impl Ash.Type.Enum
      @spec match?(term) :: boolean
      def match?(term) do
        case match(term) do
          {:ok, _} -> true
          _ -> false
        end
      end

      @impl Ash.Type.Enum
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
  def build_values_map(values) do
    values
    |> verify_values!()
    |> Enum.reduce(%{}, fn
      {value, details}, acc when is_list(details) ->
        details =
          if Keyword.has_key?(details, :label) do
            details
          else
            Keyword.put(details, :label, humanize(value))
          end

        Map.put(acc, value, Map.new(details))

      {value, description}, acc ->
        Map.put(acc, value, %{description: description, label: humanize(value)})

      value_with_no_description, acc ->
        Map.put(acc, value_with_no_description, %{
          description: nil,
          label: humanize(value_with_no_description)
        })
    end)
  end

  @doc false
  def build_values_list(values) do
    values
    |> verify_values!()
    |> Enum.map(fn
      {value, _} -> value
      value -> value
    end)
  end

  defp humanize(value) when is_atom(value) do
    value
    |> to_string()
    |> humanize()
  end

  defp humanize(value) when is_binary(value) do
    value
    |> String.replace(~r([^A-Za-z]), " ")
    |> String.capitalize()
  end

  defp humanize(value), do: value

  @doc false
  def verify_values!(values) when is_list(values) do
    Enum.each(values, fn
      value when is_atom(value) or is_binary(value) ->
        :ok

      {value, nil} when is_atom(value) or is_binary(value) ->
        :ok

      {value, description} when (is_atom(value) or is_binary(value)) and is_binary(description) ->
        :ok

      {value, details} when (is_atom(value) or is_binary(value)) and is_list(details) ->
        unsupported_opts =
          Enum.filter(details, fn {key, _} -> key not in [:description, :label] end)

        if Enum.empty?(unsupported_opts) do
          :ok
        else
          raise "Invalid value details for #{inspect(value)}: #{inspect(unsupported_opts)}.  Only `:description` and `:label` are supported."
        end

      other ->
        raise(
          "`values` must be a list of `atom | string` or {`atom | string`, string} or {`atom | string`, [description: string, label: string]} tuples, got #{inspect(other)}"
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
