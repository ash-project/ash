# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Options.Validator do
  @moduledoc """
  Defines a validator module for an option schema.

  Validators create structs with keys for each option in their schema,
  and an optimized `validate`, and `validate!` function on that struct.

  ## Upgrading from options lists

  You can pass the option `define_deprecated_access?: true` to `use Spark.Options.Validator`,
  which will make it such that `options[:foo]` will still work, but will emit a deprecation warning.
  This cane help with smoother upgrades.

  ## Example

  Given a module like the following:

  ```elixir
  defmodule MyOptions do
    use Spark.Options.Validator, schema: [
      foo: [
        type: :string,
        required: true
      ],
      bar: [
        type: :string
      ],
      baz: [
        type: :integer,
        default: 10
      ]
    ]
  end
  ```

  You can use it like so:

  ```elixir
  @doc \"\"\"
  Does a thing

  ## Options

  \#{MyOptions.docs()}
  \"""
  @doc spark_opts: [{1, MyOptions.schema()}]
  def your_function(arg, opts \\\\ []) do
    options = MyOptions.validate!(opts)

    options.foo
    options.bar
  end
  ```
  """

  defmacro __using__(opts) do
    schema = opts[:schema]
    define_deprecated_access? = opts[:define_deprecated_access?]

    [
      quote bind_quoted: [schema: schema, define_deprecated_access?: define_deprecated_access?] do
        schema = Spark.Options.new!(schema).schema
        @schema Keyword.new(schema)

        struct_fields =
          Keyword.new(@schema, fn {key, config} ->
            case Keyword.fetch(config, :default) do
              {:ok, default} ->
                case Spark.Options.validate_single_value(config[:type], key, default) do
                  {:ok, default} ->
                    {key, default}

                  {:error, error} ->
                    raise error
                end

              :error ->
                {key, nil}
            end
          end)

        @defaults @schema
                  |> Enum.filter(fn {_key, config} ->
                    Keyword.has_key?(config, :default)
                  end)
                  |> Enum.map(&elem(&1, 0))

        defstruct struct_fields ++ [__set__: @defaults]

        schema_specs = Spark.Options.Docs.schema_specs(@schema, true)

        quote do
          @type t :: %__MODULE__{unquote_splicing(schema_specs)}
        end
        |> Code.eval_quoted([], __ENV__)

        @type options :: [unquote_splicing(schema_specs)]

        required_fields =
          Spark.Options.Validator.validate_schema!(@schema)

        @required @schema
                  |> Enum.filter(fn {_key, config} ->
                    config[:required]
                  end)
                  |> Enum.map(&elem(&1, 0))

        @valid_options schema
                       |> Enum.reject(fn {_key, config} ->
                         config[:private?]
                       end)
                       |> Enum.map(&elem(&1, 0))

        @spec schema :: Spark.Options.schema()
        def schema do
          @schema
        end

        @spec docs(Keyword.t()) :: String.t()
        @spec docs() :: String.t()
        def docs(opts \\ []) do
          Spark.Options.docs(@schema |> Keyword.take(@valid_options), opts)
        end

        if define_deprecated_access? do
          def fetch(%__MODULE__{} = data, key) do
            Spark.Warning.warn_deprecated(
              "Accessing options from #{__MODULE__}",
              "Use `opts.#{key}` instead."
            )

            Map.fetch(data, key)
          end
        end

        @spec to_options(t(), Keyword.t() | nil) :: options()
        @spec to_options(t()) :: options()
        def to_options(self, take \\ nil)

        def to_options(self, nil) do
          Enum.reduce(self.__set__, [], fn key, acc ->
            [{key, Map.get(self, key)} | acc]
          end)
        end

        def to_options(self, take) do
          self
          |> to_options()
          |> Keyword.take(take)
        end

        @spec validate!(options()) :: t() | no_return
        def validate!(%__MODULE__{} = opts), do: opts

        def validate!(options) do
          Enum.reduce(options, {%__MODULE__{}, @required}, fn {key, value}, acc ->
            case validate_option(key, value, acc) do
              {:cont, {struct, missing}} ->
                {mark_set(struct, key), missing}

              {:halt, {:error, error}} ->
                raise error
            end
          end)
          |> case do
            {schema, []} ->
              schema

            {_schema, missing} ->
              raise %Spark.Options.ValidationError{
                key: missing,
                message: "Missing required keys: #{inspect(missing)}"
              }
          end
        end

        @spec validate(options) :: {:ok, t()} | {:error, term()}
        def validate(%__MODULE__{} = opts), do: {:ok, opts}

        def validate(options) do
          Enum.reduce_while(options, {%__MODULE__{}, @required}, fn {key, value}, acc ->
            case validate_option(key, value, acc) do
              {:cont, {struct, missing}} -> {:cont, {mark_set(struct, key), missing}}
              {:halt, {:error, error}} -> {:halt, {:error, error}}
            end
          end)
          |> case do
            {:error, error} ->
              {:error, error}

            {schema, []} ->
              {:ok, schema}

            {_schema, missing} ->
              {:error,
               %Spark.Options.ValidationError{
                 key: missing,
                 message: "Missing required keys: #{inspect(missing)}"
               }}
          end
        end
      end,
      quote bind_quoted: [schema: schema] do
        @compile {:inline, validate_option: 3, use_key: 2, mark_set: 2, warn_deprecated: 1}
        for {key, config} <- Keyword.new(schema) do
          type = Macro.escape(config[:type])

          cond do
            config[:private?] ->
              defp validate_option(unquote(key), value, {acc, required_fields}) do
                {:cont, {acc, required_fields}}
              end

            # we can add as many of these as we like to front load validations
            type == :integer ->
              defp validate_option(unquote(key), value, {acc, required_fields})
                   when is_integer(value) do
                {:cont, {%{acc | unquote(key) => value}, use_key(required_fields, unquote(key))}}
              end

              defp validate_option(unquote(key), value, _) do
                {:halt,
                 {:error,
                  %Spark.Options.ValidationError{
                    key: unquote(key),
                    message:
                      "invalid value for #{Spark.Options.render_key(unquote(key))}: expected integer, got: #{inspect(value)}",
                    value: value
                  }}}
              end

            type == :string ->
              defp validate_option(unquote(key), value, {acc, required_fields})
                   when is_binary(value) do
                {:cont, {%{acc | unquote(key) => value}, use_key(required_fields, unquote(key))}}
              end

              defp validate_option(unquote(key), value, _) do
                {:halt,
                 {:error,
                  %Spark.Options.ValidationError{
                    key: unquote(key),
                    message:
                      "invalid value for #{Spark.Options.render_key(unquote(key))}: expected integer, got: #{inspect(value)}",
                    value: value
                  }}}
              end

            true ->
              defp validate_option(unquote(key), value, {acc, required_fields}) do
                case Spark.Options.validate_single_value(unquote(type), unquote(key), value) do
                  {:ok, value} ->
                    {:cont,
                     {%{acc | unquote(key) => value}, use_key(required_fields, unquote(key))}}

                  {:error, error} ->
                    {:halt, {:error, error}}
                end
              end
          end
        end

        defp validate_option(unknown_key, value, _) do
          {:halt,
           {:error,
            %Spark.Options.ValidationError{
              key: unknown_key,
              message:
                "unknown options #{inspect([unknown_key])}, valid options are: #{inspect(@valid_options)}",
              value: value
            }}}
        end

        for {key, config} <- Keyword.new(schema), !config[:private?] do
          if config[:required] do
            defp use_key(list, unquote(key)) do
              warn_deprecated(unquote(key))
              List.delete(list, unquote(key))
            end
          else
            defp use_key(list, unquote(key)) do
              warn_deprecated(unquote(key))

              list
            end
          end
        end

        for {key, config} <- Keyword.new(schema),
            Keyword.has_key?(config, :default),
            !config[:private?] do
          defp mark_set(struct, unquote(key)) do
            struct
          end
        end

        defp mark_set(struct, key) do
          %{struct | __set__: [key | struct.__set__]}
        end

        for {key, config} <- Keyword.new(schema), config[:deprecated] do
          defp warn_deprecated(unquote(key)) do
            Spark.Warning.warn_deprecated(
              "#{unquote(key)}",
              "Use `opts.#{unquote(key)}` instead."
            )
          end
        end

        defp warn_deprecated(_key) do
          :ok
        end
      end
    ]
  end

  @doc false
  def validate_schema!(schema) do
    if schema[:*] do
      raise "Schema with * not supported in validator"
    else
      Enum.each(schema, fn {_key, config} ->
        if config[:type] == :keyword do
          # When we support nested keywords, they should get structs
          # auto defined for them as well.
          raise "Nested keywords not accepted in validators yet"
        end
      end)
    end
  end
end
