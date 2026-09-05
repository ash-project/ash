# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.String do
  @constraints [
    max_length: [
      type: :non_neg_integer,
      doc: "Enforces a maximum length on the value"
    ],
    min_length: [
      type: :non_neg_integer,
      doc: "Enforces a minimum length on the value"
    ],
    length_count: [
      type: {:one_of, [:graphemes, :codepoints, :bytes]},
      doc: """
      The unit used by `min_length` and `max_length`. Defaults to the unit implied by
      `config :ash, :default_string_length_count` (`:codepoints`, or `:graphemes` for `:mixed`).

      `:graphemes` matches `String.length/1`. `:codepoints` matches how most SQL data layers count
      string length. `:bytes` matches `byte_size/1`.

      A single grapheme may contain an unbounded number of codepoints (a base character followed by
      many combining marks), so `:graphemes` places no effective limit on the size of a value.
      Prefer `:codepoints` or `:bytes` when `max_length` is used as a storage or safety limit.

      Data layers cannot count graphemes, so with an explicit `:graphemes` the length constraints
      can only be applied to literal values, not atomically to expressions.
      """
    ],
    match: [
      type: :regex_as_mfa,
      doc: "Enforces that the string matches a passed in regex"
    ],
    trim?: [
      type: :boolean,
      doc: "Trims the value.",
      default: true
    ],
    allow_empty?: [
      type: :boolean,
      doc: "If false, the value is set to `nil` if it's empty.",
      default: false
    ]
  ]

  @moduledoc """
  Stores a string in the database.

  A built-in type that can be referenced via `:string`.

  By default, values are trimmed and empty values are set to `nil`.
  You can use the `allow_empty?` and `trim?` constraints to change these behaviors.

  ### Constraints

  #{Spark.Options.docs(@constraints)}
  """
  use Ash.Type
  import Ash.Gettext

  require Ash.Expr

  @impl true
  def storage_type(_), do: :string

  @impl true
  def matches_type?(v, _) do
    is_binary(v)
  end

  @impl true
  def cast_atomic(expr, constraints) when is_binary(expr) do
    with {:ok, value} <- cast_input(expr, constraints) do
      {:atomic, value}
    end
  end

  @impl true
  def cast_atomic(expr, constraints) do
    # We can't support `match` currently, as we don't have a multi-target regex
    cond do
      constraints[:match] ->
        {:not_atomic, "cannot use the `match` string constraint atomically with an expression"}

      length_constrained?(constraints) and explicit_length_count(constraints) == :graphemes ->
        {:not_atomic,
         "cannot count graphemes atomically with an expression. Set `length_count: :codepoints` or `length_count: :bytes`, or provide a literal value"}

      true ->
        expr =
          if constraints[:trim?] do
            Ash.Expr.expr(string_trim(^expr))
          else
            expr
          end

        expr =
          if constraints[:allow_empty?] do
            expr
          else
            Ash.Expr.expr(
              if ^expr == "" do
                nil
              else
                ^expr
              end
            )
          end

        {:atomic, expr}
    end
  end

  defp length_constrained?(constraints) do
    not is_nil(constraints[:max_length]) or not is_nil(constraints[:min_length])
  end

  @doc false
  # The unit to use for atomic expressions: the explicitly chosen constraint, or
  # `:codepoints` when configured. `nil` means `:mixed`, in which case atomic
  # expressions keep the legacy behavior of `string_length/1`, whatever the data
  # layer counts.
  def explicit_length_count(constraints) do
    Keyword.get(constraints, :length_count) ||
      case length_count_config() do
        :codepoints -> :codepoints
        :mixed -> nil
      end
  end

  @doc false
  # Builds the `string_length` expression used by atomic length constraints.
  def atomic_length_expr(expr, constraints) do
    case explicit_length_count(constraints) do
      nil -> Ash.Expr.expr(string_length(^expr))
      count -> Ash.Expr.expr(string_length(^expr, ^count))
    end
  end

  @impl true
  def apply_atomic_constraints(expr, constraints) do
    if Ash.Expr.expr?(expr) do
      length = atomic_length_expr(expr, constraints)

      validated =
        case {constraints[:max_length], constraints[:min_length]} do
          {nil, nil} ->
            expr

          {max, nil} ->
            Ash.Expr.expr(
              if ^length > ^max do
                error(
                  Ash.Error.Changes.InvalidChanges,
                  message: ^error_message("length must be less than or equal to %{max}"),
                  vars: %{max: ^max}
                )
              else
                ^expr
              end
            )

          {nil, min} ->
            Ash.Expr.expr(
              if ^length < ^min do
                error(
                  Ash.Error.Changes.InvalidChanges,
                  message: ^error_message("length must be greater than or equal to %{min}"),
                  vars: %{min: ^min}
                )
              else
                ^expr
              end
            )

          {max, min} ->
            Ash.Expr.expr(
              cond do
                ^length < ^min ->
                  error(
                    Ash.Error.Changes.InvalidChanges,
                    message: ^error_message("length must be greater than or equal to %{min}"),
                    vars: %{min: ^min}
                  )

                ^length > ^max ->
                  error(
                    Ash.Error.Changes.InvalidChanges,
                    message: ^error_message("length must be less than or equal to %{max}"),
                    vars: %{max: ^max}
                  )

                true ->
                  ^expr
              end
            )
        end

      {:ok, validated}
    else
      apply_constraints(expr, constraints)
    end
  end

  @impl true
  def constraints, do: @constraints

  @impl true
  def generator(constraints) do
    base_generator = length_generator(constraints)

    cond do
      constraints[:trim?] && constraints[:min_length] ->
        StreamData.filter(base_generator, fn value ->
          value
          |> String.trim()
          |> string_length(constraints)
          |> Kernel.>=(constraints[:min_length])
        end)

      constraints[:min_length] ->
        StreamData.filter(base_generator, fn value ->
          value |> string_length(constraints) |> Kernel.>=(constraints[:min_length])
        end)

      true ->
        base_generator
    end
  end

  @doc false
  # `StreamData.string/2` counts codepoints, so when counting bytes we restrict
  # the alphabet to ascii, where bytes and codepoints are the same.
  def length_generator(constraints) do
    alphabet =
      case length_count(constraints) do
        :bytes -> :ascii
        _ -> :printable
      end

    StreamData.string(alphabet, Keyword.take(constraints, [:max_length, :min_length]))
  end

  @doc false
  def length_count(constraints) do
    Keyword.get(constraints, :length_count) || default_length_count()
  end

  @doc """
  The default unit for counting string length, derived from
  `config :ash, :default_string_length_count`.

  - `:codepoints` (recommended, set by the installer) counts codepoints, matching
    SQL data layers and bounding the size of values.
  - `:mixed` keeps the previous behavior: graphemes are counted in Elixir, while
    atomic expressions defer to the data layer's own length function.

  The configuration is required. See the backwards compatibility guide for more.
  Individual attributes and validations can still choose any unit.
  """
  @spec default_length_count() :: :graphemes | :codepoints
  def default_length_count do
    case length_count_config() do
      :codepoints -> :codepoints
      :mixed -> :graphemes
    end
  end

  @doc false
  @spec length_count_config() :: :codepoints | :mixed
  def length_count_config do
    case Application.get_env(:ash, :default_string_length_count) do
      value when value in [:codepoints, :mixed] ->
        value

      other ->
        raise ArgumentError, length_count_config_error(other)
    end
  end

  @doc false
  def length_count_config_error(value) do
    intro =
      if is_nil(value) do
        "`config :ash, :default_string_length_count` is not set."
      else
        "Invalid value #{inspect(value)} for `config :ash, :default_string_length_count`."
      end

    """
    #{intro}

    Ash needs to know how to count string length for the `min_length` and `max_length`
    constraints of `:string` and `:ci_string`, for the `string_length` validation, and for
    the `string_length/1` expression. Add one of the following to `config/config.exs`:

        # Recommended. Counts unicode codepoints, which is how SQL data layers count
        # string length, so validation is consistent everywhere and `max_length`
        # bounds the size of stored values.
        config :ash, default_string_length_count: :codepoints

        # Keeps the previous behavior. Graphemes are counted when validating in
        # Elixir, while atomic updates defer to the data layer's own length function.
        # A single grapheme can contain an unbounded number of combining characters,
        # so with this setting `max_length` does not bound the size of a value.
        config :ash, default_string_length_count: :mixed

    Individual attributes can override the default with the `length_count` constraint,
    and the `string_length` validation with its `count` option, using `:graphemes`,
    `:codepoints` or `:bytes`.

    See https://hexdocs.pm/ash/backwards-compatibility-config.html#default_string_length_count
    """
  end

  @doc false
  def string_length(value, constraints) when is_list(constraints) do
    Ash.Query.Function.StringLength.string_length(value, length_count(constraints))
  end

  @impl true
  def apply_constraints(nil, _), do: :ok

  def apply_constraints(value, constraints) do
    {value, errors} =
      return_value(
        Keyword.get(constraints, :allow_empty?, false),
        Keyword.get(constraints, :trim?, true),
        value,
        constraints
      )

    case errors do
      [] -> {:ok, value}
      [error] -> {:error, error}
      errors -> {:error, errors}
    end
  end

  defp return_value(false, true, value, constraints) do
    trimmed = String.trim(value)

    if trimmed == "" do
      {nil, []}
    else
      {trimmed, validate(trimmed, constraints)}
    end
  end

  defp return_value(false, false, value, constraints) do
    if String.trim(value) == "" do
      {nil, []}
    else
      {value, validate(value, constraints)}
    end
  end

  defp return_value(true, true, value, constraints) do
    trimmed = String.trim(value)
    {trimmed, validate(trimmed, constraints)}
  end

  defp return_value(true, false, value, constraints),
    do: {value, validate(value, constraints)}

  defp validate(value, constraints) do
    Enum.reduce(constraints, [], fn
      {:max_length, max_length}, errors ->
        if string_length(value, constraints) > max_length do
          [
            [
              message: error_message("length must be less than or equal to %{max}"),
              max: max_length
            ]
            | errors
          ]
        else
          errors
        end

      {:min_length, min_length}, errors ->
        if string_length(value, constraints) < min_length do
          [
            [
              message: error_message("length must be greater than or equal to %{min}"),
              min: min_length
            ]
            | errors
          ]
        else
          errors
        end

      {:match, regex}, errors ->
        if length_ok?(value, constraints) do
          regex =
            case regex do
              {m, f, a} ->
                apply(m, f, a)

              regex ->
                regex
            end

          if Regex.match?(regex, value) do
            errors
          else
            [
              [message: error_message("must match the pattern %{regex}"), regex: inspect(regex)]
              | errors
            ]
          end
        else
          errors
        end

      _, errors ->
        errors
    end)
  end

  defp length_ok?(value, constraints) do
    if length_constrained?(constraints) do
      length = string_length(value, constraints)

      (is_nil(constraints[:max_length]) or length <= constraints[:max_length]) and
        (is_nil(constraints[:min_length]) or length >= constraints[:min_length])
    else
      true
    end
  end

  @impl true
  def cast_input(%Ash.CiString{} = ci_string, constraints) do
    ci_string
    |> Ash.CiString.value()
    |> cast_input(constraints)
  end

  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(value, constraints) when is_atom(value) do
    cast_input(to_string(value), constraints)
  end

  def cast_input(value, _) do
    Ecto.Type.cast(:string, value)
  end

  @impl true
  def coerce(value, _constraints) do
    case cast_input(value, allow_empty?: true, trim?: false) do
      {:ok, value} ->
        {:ok, value}

      _ ->
        if String.Chars.impl_for(value) do
          {:ok, to_string(value)}
        else
          {:error, "could not be coerced"}
        end
    end
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, _) do
    Ecto.Type.load(:string, value)
  end

  @impl true

  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(:string, value)
  end
end
