# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Options do
  @options_schema [
    *: [
      type: :keyword_list,
      keys: [
        type: [
          type: {:custom, __MODULE__, :validate_type, []},
          default: :any,
          doc: "The type of the option item."
        ],
        required: [
          type: :boolean,
          default: false,
          doc: "Defines if the option item is required."
        ],
        default: [
          type: :any,
          doc: """
          The default value for the option item if that option is not specified. This value
          is *validated* according to the given `:type`. This means that you cannot
          have, for example, `type: :integer` and use `default: "a string"`.
          """
        ],
        keys: [
          type: :keyword_list,
          doc: """
          Available for types `:keyword_list`, `:non_empty_keyword_list`, and `:map`,
          it defines which set of keys are accepted for the option item. The value of the
          `:keys` option is a schema itself. For example: `keys: [foo: [type: :atom]]`.
          Use `:*` as the key to allow multiple arbitrary keys and specify their schema:
          `keys: [*: [type: :integer]]`.
          """,
          keys: &__MODULE__.options_schema/0
        ],
        deprecated: [
          type: :string,
          doc: """
          Defines a message to indicate that the option item is deprecated. \
          The message will be displayed as a warning when passing the item.
          """
        ],
        private?: [
          type: :boolean,
          default: false,
          doc: "Defines an option as private, used with `Spark.Options.Validator`"
        ],
        hide: [
          type: {:wrap_list, :atom},
          doc: """
          A list of keys that should be hidden when generating documentation
          """
        ],
        as: [
          type: :atom,
          doc:
            "A name to remap the option to when used in DSLs. Not supported in regular option parsing"
        ],
        snippet: [
          type: :string,
          doc:
            "A snippet to use when autocompleting DSLs. Not supported in regular option parsing"
        ],
        links: [
          type: :any,
          doc: "A keyword list of links to include in DSL documentation for the option item."
        ],
        doc: [
          type: {:or, [:string, {:in, [false]}]},
          type_doc: "`t:String.t/0` or `false`",
          doc: "The documentation for the option item."
        ],
        subsection: [
          type: :string,
          doc: "The title of separate subsection of the options' documentation"
        ],
        type_doc: [
          type: {:or, [:string, {:in, [false]}]},
          type_doc: "`t:String.t/0` or `false`",
          doc: """
          The type doc to use *in the documentation* for the option item. If `false`,
          no type documentation is added to the item. If it's a string, it can be
          anything. For example, you can use `"a list of PIDs"`, or you can use
          a typespec reference that ExDoc can link to the type definition, such as
          `` "`t:binary/0`" ``. You can use Markdown in this documentation. If the
          `:type_doc` option is not present, Spark.Options tries to produce a type
          documentation automatically if it can do it unambiguously. For example,
          if `type: :integer`, Spark.Options will use `t:integer/0` as the
          auto-generated type doc.
          """
        ],
        type_spec: [
          type: :any,
          type_doc: "`t:Macro.t/0`",
          doc: """
          The quoted spec to use *in the typespec* for the option item. You should use this
          when the auto-generated spec is not specific enough. For example, if you are performing
          custom validation on an option (with the `{:custom, ...}` type), then the
          generated type spec for that option will always be `t:term/0`, but you can use
          this option to customize that. The value for this option **must** be a quoted Elixir
          term. For example, if you have an `:exception` option that is validated with a
          `{:custom, ...}` type (based on `is_exception/1`), you can override the type
          spec for that option to be `quote(do: Exception.t())`. *Available since v1.1.0*.
          """
        ]
      ]
    ]
  ]

  @moduledoc """
  Provides a standard API to handle keyword-list-based options.

  This module began its life as a vendored form of `NimbleOptions`,
  meaning that we copied it from `NimbleOptions` into `Spark`.
  We had various features to add to it, and the spirit of nimble
  options is to be as lightweight as possible. With that in mind,
  we were advised to vendor it. We would like to thank the authors
  of `NimbleOptions` for their excellent work, and their blessing
  to transplant their work into Spark.

  `Spark.Options` allows developers to create schemas using a
  pre-defined set of options and types. The main benefits are:

    * A single unified way to define simple static options
    * Config validation against schemas
    * Automatic doc generation
    * More types over what is provided by `NimbleOptions`
    * Compile time validators that are highly optimized and produce structs. See `Spark.Options.Validator`.
    * Shared logic between Spark DSLs and options lists.

  ## Schema Options

  These are the options supported in a *schema*. They are what
  defines the validation for the items in the given schema.

  #{Spark.Options.Docs.generate(@options_schema, nest_level: 0)}

  ## Types

    * `:any` - Any type.

    * `:keyword_list` - A keyword list.

    * `:non_empty_keyword_list` - A non-empty keyword list.

    * `{:keyword_list, schema}` - A keyword list matching the given options schema.

    * `:non_empty_keyword_list` - A non-empty keyword list.

    * `{:non_empty_keyword_list, schema}` - A non-empty keyword list matching the given options schema.

    * `:map` - A map consisting of `:atom` keys. Shorthand for `{:map, :atom, :any}`.
      Keys can be specified using the `keys` option.

    * `{:map, key_type, value_type}` - A map consisting of `key_type` keys and
      `value_type` values.

    * `:atom` - An atom.

    * `:string` - A string.

    * `:boolean` - A boolean.

    * `:integer` - An integer.

    * `:non_neg_integer` - A non-negative integer.

    * `:pos_integer` - A positive integer (greater than zero).

    * `:float` - A float.

    * `:number` - An integer or a float.

    * `:timeout` - A non-negative integer or the atom `:infinity`.

    * `:pid` - A PID (process identifier).

    * `:reference` - A reference (see `t:reference/0`).

    * `nil` - The value `nil` itself. Available since v1.0.0.

    * `:mfa` - A named function in the format `{module, function, arity}` where
      `arity` is a list of arguments. For example, `{MyModule, :my_fun, [arg1, arg2]}`.

    * `:mod_arg` - A module along with arguments, such as `{MyModule, arguments}`.
      Usually used for process initialization using `start_link` and similar. The
      second element of the tuple can be any term.

    * `:regex` - A regex

    * `:regex_as_mfa` - A regex pattern that gets converted to an MFA tuple for caching.
      Accepts a compiled regex (`~r/pattern/flags`), a string pattern (`"pattern"`), or a
      tuple of pattern and flags (`{"pattern", "flags"}`) and converts it to
      `{Spark.Regex, :cache, [source, opts]}` to work around OTP 28's restriction on
      compile-time regex creation.

    * `:fun` - Any function.

    * `{:fun, arity}` - Any function with the specified arity.

    * `{:fun, args_types}` - A function with the specified arguments.

    * `{:fun, args_types, return_type}` - A function with the specified arguments and return type.

    * `{:in, choices}` or `{:one_of, choices}` - A value that is a member of one of the `choices`. `choices`
      should be a list of terms or a `Range`. The value is an element in said
      list of terms, that is, `value in choices` is `true`.

    * `{:struct, struct_name}` - An instance of the struct type given.

    * `:struct` - An instance of any struct

    * `{:tagged_tuple, tag, inner_type}` - maps to `{tag, type}`

    * `{:spark_behaviour, behaviour}` - expects a module that implements the given behaviour, and can be specified with options, i.e `mod` or `{mod, [opt: :val]}`

    * `{:spark_behaviour, behaviour, builtin_module}` - Same as the above, but also accepts a `builtin_module`. The builtin_module is used to provide additional options for the elixir_sense plugin.

    * `{:spark_function_behaviour, behaviour, {function_mod, arity}}` - expects a module that implements the given behaviour, and can be specified with options, i.e `mod` or `{mod, [opt: :val]}`, that also has a special module that supports being provided an anonymous function or MFA as the `:fun` option.

    * `{:spark_function_behaviour, behaviour, builtin_module, {function_mod, arity}}` - Same as the above, but also accepts a `builtin_module`. The builtin_module is used to provide additional options for the elixir_sense plugin.

    * `{:behaviour, behaviour}` - expects a module that implements a given behaviour.

    * `{:protocol, protocol}` - expects a value for which the protocol is implemented.

    * `{:impl, protocol}` - expects a module for which the protocol is implemented.

    * `{:spark, dsl_module}` - expects a module that is a `Spark.Dsl`

    * `{:mfa_or_fun, arity}` - expects a function or MFA of a corresponding arity.

    * `{:spark_type, module, builtin_function}` - a behaviour that defines `builtin_function/0` that returns a list of atoms that map to built in variations of that thing.

    * `{:spark_type, module, builtin_function, templates}` - same as the above, but includes additional templates for elixir_sense autocomplete

    * `:literal` -> any literal value. Maps to `:any`, but is used for documentation.

    * `{:literal, value}` -> exactly the value specified.

    * `:quoted` -> retains the quoted value of the code provided to the option

    * `{:wrap_list, type}` -> Allows a single value or a list of values.

    * `{:custom, mod, fun, args}` - A custom type. The related value must be validated
      by `mod.fun(values, ...args)`. The function should return `{:ok, value}` or
      `{:error, message}`. `args` allow for passing static arguments to the function. If
      the list is empty, the function must have exactly one argument, i.e. `{:custom, mod, fun, []}`
      expects `mod.fun/1` to exist.

    * `{:and, subtypes}` - A value that matches all of the given `subtypes`. The value is
      matched against the subtypes in the order specified in the list of `subtypes`. If
      all of the subtypes match then the value is valid. If one of the subtypes matches and **updates** (casts) a given value, then value is updated and
      passed in to any subsequent checks.
      If one of the subtypes is a keyword list or map, you won't be able to pass
      `:keys` directly. For this reason `:keyword_list`, `:non_empty_keyword_list`,
      and `:map` are special cased and can be used as subtypes with
      `{:keyword_list, keys}`, `{:non_empty_keyword_list, keys}` or `{:map, keys}`.

    * `{:or, subtypes}` - A value that matches one of the given `subtypes`. The value is
      matched against the subtypes in the order specified in the list of `subtypes`. If
      one of the subtypes matches and **updates** (casts) the given value, the updated
      value is used. For example: `{:or, [:string, :boolean, {:fun, 2}]}`. If one of the
      subtypes is a keyword list or map, you won't be able to pass `:keys` directly. For this reason,
      `:keyword_list`, `:non_empty_keyword_list`, and `:map` are special cased and can
      be used as subtypes with `{:keyword_list, keys}`, `{:non_empty_keyword_list, keys}` or `{:map, keys}`.
      For example, a type such as `{:or, [:boolean, keyword_list: [enabled: [type: :boolean]]]}`
      would match either a boolean or a keyword list with the `:enabled` boolean option in it.

    * `{:list, subtype}` - A list where all elements match `subtype`. `subtype` can be any
      of the accepted types listed here. Empty lists are allowed. The resulting validated list
      contains the validated (and possibly updated) elements, each as returned after validation
      through `subtype`. For example, if `subtype` is a custom validator function that returns
      an updated value, then that updated value is used in the resulting list. Validation
      fails at the *first* element that is invalid according to `subtype`. If `subtype` is
      a keyword list or map, you won't be able to pass `:keys` directly. For this reason,
      `:keyword_list`, `:non_empty_keyword_list`, and `:map` are special cased and can
      be used as the subtype by using `{:keyword_list, keys}`, `{:non_empty_keyword_list, keys}`
      or `{:keyword_list, keys}`. For example, a type such as
      `{:list, {:keyword_list, enabled: [type: :boolean]}}` would a *list of keyword lists*,
      where each keyword list in the list could have the `:enabled` boolean option in it.

    * `{:tuple, list_of_subtypes}` - A tuple as described by `tuple_of_subtypes`.
      `list_of_subtypes` must be a list with the same length as the expected tuple.
      Each of the list's elements must be a subtype that should match the given element in that
      same position. For example, to describe 3-element tuples with an atom, a string, and
      a list of integers you would use the type `{:tuple, [:atom, :string, {:list, :integer}]}`.
      *Available since v0.4.1*.

  ## Example

      iex> schema = [
      ...>   producer: [
      ...>     type: :non_empty_keyword_list,
      ...>     required: true,
      ...>     keys: [
      ...>       module: [required: true, type: :mod_arg],
      ...>       concurrency: [
      ...>         type: :pos_integer,
      ...>       ]
      ...>     ]
      ...>   ]
      ...> ]
      ...>
      ...> config = [
      ...>   producer: [
      ...>     concurrency: 1,
      ...>   ]
      ...> ]
      ...>
      ...> {:error, %Spark.Options.ValidationError{} = error} = Spark.Options.validate(config, schema)
      ...> Exception.message(error)
      "required :module option not found, received options: [:concurrency] (in options [:producer])"

  ## Nested Option Items

  `Spark.Options` allows option items to be nested so you can recursively validate
  any item down the options tree.

  ### Example

      iex> schema = [
      ...>   producer: [
      ...>     required: true,
      ...>     type: :non_empty_keyword_list,
      ...>     keys: [
      ...>       rate_limiting: [
      ...>         type: :non_empty_keyword_list,
      ...>         keys: [
      ...>           interval: [required: true, type: :pos_integer]
      ...>         ]
      ...>       ]
      ...>     ]
      ...>   ]
      ...> ]
      ...>
      ...> config = [
      ...>   producer: [
      ...>     rate_limiting: [
      ...>       interval: :oops!
      ...>     ]
      ...>   ]
      ...> ]
      ...>
      ...> {:error, %Spark.Options.ValidationError{} = error} = Spark.Options.validate(config, schema)
      ...> Exception.message(error)
      "invalid value for :interval option: expected positive integer, got: :oops! (in options [:producer, :rate_limiting])"

  ## Validating Schemas

  Each time `validate/2` is called, the given schema itself will be validated before validating
  the options.

  In most applications the schema will never change but validating options will be done
  repeatedly.

  To avoid the extra cost of validating the schema, it is possible to validate the schema once,
  and then use that valid schema directly. This is done by using the `new!/1` function first, and
  then passing the returned schema to `validate/2`.

  > #### Create the Schema at Compile Time {: .tip}
  >
  > If your option schema doesn't include any runtime-only terms in it (such as anonymous
  > functions), you can call `new!/1` to validate the schema and returned a *compiled* schema
  > **at compile time**. This is an efficient way to avoid doing any unnecessary work at
  > runtime. See the example below for more information.

  ### Example

      iex> raw_schema = [
      ...>   hostname: [
      ...>     required: true,
      ...>     type: :string
      ...>   ]
      ...> ]
      ...>
      ...> schema = Spark.Options.new!(raw_schema)
      ...> Spark.Options.validate([hostname: "elixir-lang.org"], schema)
      {:ok, hostname: "elixir-lang.org"}

  Calling `new!/1` from a function that receives options will still validate the schema each time
  that function is called. Declaring the schema as a module attribute is supported:

      @options_schema Spark.Options.new!([...])

  This schema will be validated at compile time. Calling `docs/1` on that schema is also
  supported.
  """

  alias Spark.Options.ValidationError

  defstruct schema: []

  @basic_types [
    :any,
    :keyword_list,
    :non_empty_keyword_list,
    :map,
    :atom,
    :integer,
    :non_neg_integer,
    :pos_integer,
    :float,
    :number,
    :module,
    :mfa,
    :mod_arg,
    :regex_as_mfa,
    :regex,
    :string,
    :boolean,
    :timeout,
    :fun,
    :pid,
    :reference,
    :literal,
    :quoted,
    :struct,
    nil
  ]

  @type type ::
          :any
          | :keyword_list
          | :non_empty_keyword_list
          | :map
          | {:map, key_type :: type, value_type :: type}
          | :atom
          | :string
          | :boolean
          | :integer
          | :non_neg_integer
          | :pos_integer
          | :float
          | :number
          | :timeout
          | :pid
          | :reference
          | :mfa
          | :mod_arg
          | :fun
          | {:fun, arity :: non_neg_integer}
          | {:fun, list(type)}
          | {:fun, list(type), type}
          | {:in, [any] | Range.t()}
          | {:and,
             [type | {:keyword_list, schema} | {:non_empty_keyword_list, schema} | {:map, schema}]}
          | {:or,
             [type | {:keyword_list, schema} | {:non_empty_keyword_list, schema} | {:map, schema}]}
          | {:list,
             type | {:keyword_list, schema} | {:non_empty_keyword_list, schema} | {:map, schema}}
          | {:tuple, [type]}
          | {:one_of, [any] | Range.t()}
          | {:tagged_tuple, tag :: atom, inner_type :: type}
          | {:spark_behaviour, module}
          | {:spark_behaviour, module, module}
          | {:spark_function_behaviour, module, {module, integer}}
          | {:spark_function_behaviour, module, module, {module, integer}}
          | {:behaviour, module}
          | {:protocol, module}
          | {:impl, module}
          | {:spark, module}
          | {:mfa_or_fun, non_neg_integer()}
          | {:spark_type, module, builtin_function :: atom}
          | {:spark_type, module, builtin_function :: atom, templates :: [String.t()]}
          | {:struct, module}
          | {:wrap_list, type}
          | :literal
          | {:literal, any}
          | :quoted
          | {:custom, module, function :: atom, args :: [any]}

  @type option_schema :: [
          {:type, type}
          | {:required, boolean}
          | {:default, any}
          | {:keys, schema}
          | {:private?, boolean}
          | {:deprecated, String.t()}
          | {:doc, String.t()}
          | {:subsection, String.t() | nil}
          | {:type_doc, false | String.t()}
          | {:rename_to, atom}
          | {:hide, [atom]}
          | {:as, atom}
          | {:snippet, String.t()}
          | {:links, keyword()}
        ]

  @typedoc """
  A schema.

  See the module documentation for more information.
  """
  @type schema :: [{atom, option_schema}]

  @typedoc """
  The `Spark.Options` struct embedding a validated schema.

  See the [*Validating Schemas* section](#module-validating-schemas) in
  the module documentation.
  """
  @type t() :: %__MODULE__{schema: schema()}

  @doc """
  Validates the given `options` with the given `schema`.

  See the module documentation for what a `schema` is.

  If the validation is successful, this function returns `{:ok, validated_options}`
  where `validated_options` is a keyword list. If the validation fails, this
  function returns `{:error, validation_error}` where `validation_error` is a
  `Spark.Options.ValidationError` struct explaining what's wrong with the options.
  You can use `raise/1` with that struct or `Exception.message/1` to turn it into a string.

  ## Examples


      iex> Spark.Options.validate(
      ...>   [
      ...>     a: 123,
      ...>     b: 4.2,
      ...>     c: :"",
      ...>     d: "a string"
      ...>   ],
      ...>   [
      ...>     a: [type: :pos_integer],
      ...>     b: [type: :number],
      ...>     c: [type: :atom],
      ...>     d: [type: :string]
      ...>   ]
      ...> )
      {:ok, [a: 123, b: 4.2, c: :"", d: "a string"]}

      iex> Spark.Options.validate(
      ...>   [
      ...>     a: 0,
      ...>     b: -13,
      ...>   ],
      ...>   [
      ...>     a: [type: :pos_integer],
      ...>     b: [type: :string]
      ...>   ]
      ...> )
      {:error,
       %Spark.Options.ValidationError{
         message: "invalid value for :a option: expected positive integer, got: 0",
         key: :a,
         value: 0,
         keys_path: []
       }}


  """
  @spec validate(keyword(), schema() | t()) ::
          {:ok, validated_options :: keyword()} | {:error, ValidationError.t()}

  def validate(options, %Spark.Options{schema: schema}) do
    validate_options_with_schema(options, schema)
  end

  def validate(options, schema) when is_list(options) and is_list(schema) do
    validate(options, new!(schema))
  end

  @doc """
  Merges two schemas, and sets the `subsection` option on all options on the right side.
  """
  @spec merge(schema(), schema(), String.t() | nil) :: schema()
  def merge(left, right, section \\ nil) do
    new_right =
      Enum.map(right, fn {key, value} ->
        {key, Keyword.put(value, :subsection, section)}
      end)

    left ++ new_right
  end

  @doc """
  Validates the given `options` with the given `schema` and raises if they're not valid.

  This function behaves exactly like `validate/2`, but returns the options directly
  if they're valid or raises a `Spark.Options.ValidationError` exception otherwise.
  """
  @spec validate!(keyword(), schema() | t()) :: validated_options :: keyword()
  def validate!(options, schema) do
    case validate(options, schema) do
      {:ok, options} -> options
      {:error, %ValidationError{} = error} -> raise error
    end
  end

  @doc """
  Validates the given `schema` and returns a wrapped schema to be used with `validate/2`.

  If the given schema is not valid, raises a `Spark.Options.ValidationError`.
  """
  @spec new!(schema()) :: t()
  def new!(schema) when is_list(schema) do
    case validate_options_with_schema(schema, options_schema()) do
      {:ok, validated_schema} ->
        %Spark.Options{schema: validated_schema}

      {:error, %ValidationError{} = error} ->
        raise ArgumentError,
              "invalid Spark.Options schema. Reason: #{Exception.message(error)}"
    end
  end

  @doc ~S"""
  Returns documentation for the given schema.

  You can use this to inject documentation in your docstrings. For example,
  say you have your schema in a module attribute:

      @options_schema [...]

  With this, you can use `docs/1` to inject documentation:

      @doc "Supported options:\n#{Spark.Options.docs(@options_schema)}"

  ## Options

    * `:nest_level` - an integer deciding the "nest level" of the generated
      docs. This is useful when, for example, you use `docs/2` inside the `:doc`
      option of another schema. For example, if you have the following nested schema:

          nested_schema = [
            allowed_messages: [type: :pos_integer, doc: "Allowed messages."],
            interval: [type: :pos_integer, doc: "Interval."]
          ]

      then you can document it inside another schema with its nesting level increased:

          schema = [
            producer: [
              type: {:or, [:string, keyword_list: nested_schema]},
              doc:
                "Either a string or a keyword list with the following keys:\n\n" <>
                  Spark.Options.docs(nested_schema, nest_level: 1)
            ],
            other_key: [type: :string]
          ]

  """
  @spec docs(schema() | t(), keyword()) :: String.t()
  def docs(schema, options \\ [])

  def docs(schema, options) when is_list(schema) and is_list(options) do
    schema
    |> Keyword.new()
    |> Enum.reject(fn {_key, opts} ->
      opts[:hide]
    end)
    |> Enum.map(fn {key, opts} ->
      {key, update_key_docs(opts)}
    end)
    |> Spark.Options.Docs.generate(options)
  end

  def docs(%Spark.Options{schema: schema}, options) when is_list(options) do
    schema
    |> Keyword.new()
    |> Enum.reject(fn {_key, opts} ->
      opts[:hide]
    end)
    |> Enum.map(fn {key, opts} ->
      {key, update_key_docs(opts)}
    end)
    |> Spark.Options.Docs.generate(options)
  end

  @doc false
  def update_key_docs(opts) do
    opts
    |> Keyword.put_new(:doc, "")
    |> Keyword.update!(:doc, &String.replace(&1, "\n\n", "  \n"))
    |> document_values()
  end

  defp document_values(opts) do
    case opts[:type] do
      {in_type, range}
      when in_type in [:in, :one_of] and is_struct(range, Range) and range.step > 0 ->
        Keyword.update!(
          opts,
          :doc,
          &"#{&1} Valid values are between #{range.first} and #{range.last}"
        )

      {in_type, range}
      when in_type in [:in, :one_of] and is_struct(range, Range) and range.step < 0 ->
        Keyword.update!(
          opts,
          :doc,
          &"#{&1} Valid values are between #{range.last} and #{range.first}"
        )

      {in_type, values} when in_type in [:in, :one_of] ->
        values = Enum.map_join(values, ", ", &inspect/1)

        Keyword.update!(
          opts,
          :doc,
          &"#{&1} Valid values are #{values}"
        )

      {list_type, {in_type, values}}
      when in_type in [:in, :one_of] and list_type in [:list, :wrap_list] ->
        values = Enum.map_join(values, ", ", &inspect/1)

        Keyword.update!(
          opts,
          :doc,
          &"#{&1} Valid values are #{values}"
        )

      _ ->
        opts
    end
  end

  @doc """
  Returns the quoted typespec for any option described by the given schema.

  The returned quoted code represents the **type union** for all possible
  keys in the schema, alongside their type. Nested keyword lists are
  spec'ed as `t:keyword/0`.

  ## Usage

  Because of how typespecs are treated by the Elixir compiler, you have
  to use `unquote/1` on the return value of this function to use it
  in a typespec:

      @type option() :: unquote(Spark.Options.option_typespec(my_schema))

  This function returns the type union for a single option: to give you
  flexibility to combine it and use it in your own typespecs. For example,
  if you only validate part of the options through Spark.Options, you could
  write a spec like this:

      @type my_option() ::
              {:my_opt1, integer()}
              | {:my_opt2, boolean()}
              | unquote(Spark.Options.option_typespec(my_schema))

  If you want to spec a whole schema, you could write something like this:

      @type options() :: [unquote(Spark.Options.option_typespec(my_schema))]

  ## Example

      schema = [
        int: [type: :integer],
        number: [type: {:or, [:integer, :float]}]
      ]

      @type option() :: unquote(Spark.Options.option_typespec(schema))

  The code above would essentially compile to:

      @type option() :: {:int, integer()} | {:number, integer() | float()}

  """
  @spec option_typespec(schema() | t()) :: Macro.t()
  def option_typespec(schema)

  def option_typespec(schema) when is_list(schema) do
    Spark.Options.Docs.schema_to_spec(schema)
  end

  def option_typespec(%Spark.Options{schema: schema}) do
    Spark.Options.Docs.schema_to_spec(schema)
  end

  @doc false
  def options_schema do
    @options_schema
  end

  defp validate_options_with_schema(opts, schema) do
    validate_options_with_schema_and_path(opts, schema, _path = [])
  end

  defp validate_options_with_schema_and_path(opts, fun, path) when is_function(fun) do
    validate_options_with_schema_and_path(opts, fun.(), path)
  end

  defp validate_options_with_schema_and_path(opts, schema, path) when is_map(opts) do
    list_opts = Map.to_list(opts)

    case validate_options_with_schema_and_path(list_opts, schema, path) do
      {:ok, validated_list_opts} -> {:ok, Map.new(validated_list_opts)}
      error -> error
    end
  end

  defp validate_options_with_schema_and_path(opts, schema, path) when is_list(opts) do
    case validate_options(schema, opts) do
      {:ok, options} ->
        {:ok, options}

      {:error, %ValidationError{} = error} ->
        {:error, %ValidationError{error | keys_path: path ++ error.keys_path}}
    end
  end

  defp validate_options(schema, opts) do
    {required, defaults} =
      Enum.reduce(schema, {[], []}, fn {key, opts}, {required, defaults} ->
        if opts[:required] do
          {[key | required], defaults}
        else
          case Keyword.fetch(opts, :default) do
            {:ok, default} ->
              case validate_type(opts[:type], key, default) do
                {:ok, default} ->
                  {required, [{key, default} | defaults]}

                {:error, error} ->
                  raise ArgumentError,
                        "Invalid spec, default value #{inspect(default)} for #{key} is invalid: #{inspect(error)}"
              end

            :error ->
              {required, defaults}
          end
        end
      end)

    Enum.reduce_while(opts, {:ok, [], required}, fn {key, value}, {:ok, validated, required} ->
      with {:ok, value, option_schema} <- validate_value(schema, opts, key, value),
           {:ok, value} <- validate_type(option_schema[:type], key, value) do
        new_required = required -- [key]

        if nested_schema = option_schema[:keys] do
          case validate_options_with_schema_and_path(value, nested_schema, _path = [key]) do
            {:ok, value} ->
              {:cont, {:ok, [{key, value} | validated], new_required}}

            {:error, error} ->
              {:halt, {:error, error}}
          end
        else
          {:cont, {:ok, [{key, value} | validated], new_required}}
        end
      else
        other ->
          {:halt, other}
      end
    end)
    |> case do
      {:ok, validated, []} ->
        case defaults do
          [] ->
            {:ok, Enum.reverse(validated)}

          _ ->
            {:ok, Enum.reverse(Keyword.merge(defaults, validated))}
        end

      {:ok, _validated, [key | _]} ->
        error_tuple(
          key,
          nil,
          "required #{render_key(key)} not found, received options: " <>
            inspect(Keyword.keys(opts))
        )

      {:error, error} ->
        {:error, error}
    end
  end

  defp validate_value(schema, opts, key, value) do
    case Keyword.fetch(schema, key) do
      {:ok, schema} ->
        if message = Keyword.get(schema, :deprecated) do
          Spark.Warning.warn_deprecated("#{render_key(key)}", message)
        end

        {:ok, value, schema}

      :error ->
        case Keyword.fetch(schema, :*) do
          :error ->
            if Keyword.get(schema, :required, false) == true do
              error_tuple(
                key,
                nil,
                "required #{render_key(key)} not found, received options: " <>
                  inspect(Keyword.keys(opts))
              )
            else
              error_tuple(
                [key],
                nil,
                "unknown options #{inspect([key])}, valid options are: #{inspect(Keyword.keys(schema))}"
              )
            end

          {:ok, default} ->
            {:ok, value, default}
        end
    end
  end

  @doc false
  def validate_single_value(type, key, value) do
    validate_type(type, key, value)
  end

  defp validate_type(:integer, key, value) when not is_integer(value) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected integer, got: #{inspect(value)}"
    )
  end

  defp validate_type(:non_neg_integer, key, value) when not is_integer(value) or value < 0 do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected non negative integer, got: #{inspect(value)}"
    )
  end

  defp validate_type(:pos_integer, key, value) when not is_integer(value) or value < 1 do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected positive integer, got: #{inspect(value)}"
    )
  end

  defp validate_type(:float, key, value) when not is_float(value) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected float, got: #{inspect(value)}"
    )
  end

  defp validate_type(:number, key, value) when not is_float(value) and not is_integer(value) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected integer or float, got: #{inspect(value)}"
    )
  end

  defp validate_type(:atom, key, value) when not is_atom(value) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected atom, got: #{inspect(value)}"
    )
  end

  defp validate_type(:timeout, key, value)
       when not (value == :infinity or (is_integer(value) and value >= 0)) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected non-negative integer or :infinity, got: #{inspect(value)}"
    )
  end

  defp validate_type(:string, key, value) when not is_binary(value) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected string, got: #{inspect(value)}"
    )
  end

  defp validate_type(:boolean, key, value) when not is_boolean(value) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected boolean, got: #{inspect(value)}"
    )
  end

  defp validate_type(:keyword_list, key, value) do
    if keyword_list?(value) do
      {:ok, value}
    else
      error_tuple(
        key,
        value,
        "invalid value for #{render_key(key)}: expected keyword list, got: #{inspect(value)}"
      )
    end
  end

  defp validate_type({:keyword_list, schema}, key, value) do
    if keyword_list?(value) do
      validate_options_with_schema_and_path(value, schema, _path = [key])
    else
      error_tuple(
        key,
        value,
        "invalid value for #{render_key(key)}: expected keyword list, got: #{inspect(value)}"
      )
    end
  end

  defp validate_type(:non_empty_keyword_list, key, value) do
    if keyword_list?(value) and value != [] do
      {:ok, value}
    else
      error_tuple(
        key,
        value,
        "invalid value for #{render_key(key)}: expected non-empty keyword list, got: #{inspect(value)}"
      )
    end
  end

  defp validate_type({:non_empty_keyword_list, schema}, key, value) do
    if keyword_list?(value) and value != [] do
      validate_options_with_schema_and_path(value, schema, _path = [key])
    else
      error_tuple(
        key,
        value,
        "invalid value for #{render_key(key)}: expected non-empty keyword list, got: #{inspect(value)}"
      )
    end
  end

  defp validate_type(:map, key, value) do
    validate_type({:map, :atom, :any}, key, value)
  end

  defp validate_type({:map, key_type, value_type}, key, map) when is_map(map) do
    map
    |> Enum.reduce_while([], fn {key, value}, acc ->
      with {:ok, updated_key} <- validate_type(key_type, {__MODULE__, :key}, key),
           {:ok, updated_value} <- validate_type(value_type, {__MODULE__, :value, key}, value) do
        {:cont, [{updated_key, updated_value} | acc]}
      else
        {:error, %ValidationError{} = error} -> {:halt, error}
      end
    end)
    |> case do
      pairs when is_list(pairs) ->
        {:ok, Map.new(pairs)}

      %ValidationError{} = error ->
        error_tuple(key, map, "invalid map in #{render_key(key)}: #{error.message}")
    end
  end

  defp validate_type({:map, _, _}, key, value) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected map, got: #{inspect(value)}"
    )
  end

  defp validate_type(:pid, _key, value) when is_pid(value) do
    {:ok, value}
  end

  defp validate_type(:pid, key, value) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected pid, got: #{inspect(value)}"
    )
  end

  defp validate_type(:reference, _key, value) when is_reference(value) do
    {:ok, value}
  end

  defp validate_type(:reference, key, value) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected reference, got: #{inspect(value)}"
    )
  end

  defp validate_type(:mfa, _key, {mod, fun, args} = value)
       when is_atom(mod) and is_atom(fun) and is_list(args) do
    {:ok, value}
  end

  defp validate_type(:mfa, key, value) when not is_nil(value) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected tuple {mod, fun, args}, got: #{inspect(value)}"
    )
  end

  defp validate_type(:regex, _key, %Regex{} = value) do
    {:ok, value}
  end

  defp validate_type(:regex, key, {value, flags}) do
    case Regex.compile(value, flags) do
      {:ok, regex} ->
        {:ok, regex}

      {:error, reason} ->
        error_tuple(
          key,
          value,
          "invalid value for #{render_key(key)}: could not compile regex. Reason: #{reason}"
        )
    end
  end

  defp validate_type(:regex, key, value) when is_binary(value) do
    case Regex.compile(value) do
      {:ok, regex} ->
        {:ok, regex}

      {:error, reason} ->
        error_tuple(
          key,
          value,
          "invalid value for #{render_key(key)}: could not compile regex. Reason: #{reason}"
        )
    end
  end

  defp validate_type(:regex, key, value) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected regex, got: #{inspect(value)}"
    )
  end

  defp validate_type(:regex_as_mfa, _key, {Spark.Regex, :cache, [source, opts]}) do
    {:ok, {Spark.Regex, :cache, [source, opts]}}
  end

  defp validate_type(:regex_as_mfa, _key, %Regex{} = regex) do
    source = Regex.source(regex)
    opts = Regex.opts(regex)
    {:ok, {Spark.Regex, :cache, [source, opts]}}
  end

  defp validate_type(:regex_as_mfa, _key, pattern) when is_binary(pattern) do
    {:ok, {Spark.Regex, :cache, [pattern, ""]}}
  end

  defp validate_type(:regex_as_mfa, _key, {pattern, flags})
       when is_binary(pattern) and is_binary(flags) do
    {:ok, {Spark.Regex, :cache, [pattern, flags]}}
  end

  defp validate_type(:regex_as_mfa, key, value) when not is_nil(value) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected regex, string, or {pattern, flags} tuple, got: #{inspect(value)}"
    )
  end

  defp validate_type(:mod_arg, _key, {mod, _arg} = value) when is_atom(mod) do
    {:ok, value}
  end

  defp validate_type(:mod_arg, key, value) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected tuple {mod, arg}, got: #{inspect(value)}"
    )
  end

  defp validate_type(:fun, _key, value) when is_function(value) do
    {:ok, value}
  end

  defp validate_type(:fun, key, value) do
    error_tuple(key, value, "expected function in #{render_key(key)}, got: #{inspect(value)}")
  end

  defp validate_type({:fun, args}, key, value) when is_list(args) do
    validate_type({:fun, length(args)}, key, value)
  end

  defp validate_type({:fun, args, _}, key, value) when is_list(args) do
    validate_type({:fun, length(args)}, key, value)
  end

  defp validate_type({:fun, arity}, _key, value) when is_function(value, arity) do
    {:ok, value}
  end

  defp validate_type({:fun, arity}, key, value) when is_function(value) do
    actual_arity = :erlang.fun_info(value, :arity)

    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected function of arity #{arity}, got: function of arity #{inspect(actual_arity)}"
    )
  end

  defp validate_type({:fun, arity}, key, value) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected function of arity #{arity}, got: #{inspect(value)}"
    )
  end

  defp validate_type(nil, _key, nil), do: {:ok, nil}

  defp validate_type(nil, key, value),
    do:
      error_tuple(
        key,
        value,
        "invalid value for #{render_key(key)}: expected nil, got: #{inspect(value)}"
      )

  defp validate_type({:custom, mod, fun, args}, key, value) do
    case apply(mod, fun, [value | args]) do
      {:ok, value} ->
        {:ok, value}

      {:error, message} when is_binary(message) ->
        error_tuple(key, value, "invalid value for #{render_key(key)}: " <> message)

      other ->
        raise "custom validation function #{inspect(mod)}.#{fun}/#{length(args) + 1} " <>
                "must return {:ok, value} or {:error, message}, got: #{inspect(other)}"
    end
  end

  defp validate_type({in_type, choices}, key, value) when in_type in [:in, :one_of] do
    if value in choices do
      {:ok, value}
    else
      error_tuple(
        key,
        value,
        "invalid value for #{render_key(key)}: expected one of #{inspect(choices)}, got: #{inspect(value)}"
      )
    end
  end

  defp validate_type({:and, subtypes}, key, value) do
    result =
      Enum.reduce_while(subtypes, {value, []}, fn subtype, {current_value, errors_acc} ->
        {subtype, nested_schema} =
          case subtype do
            {type, keys} when type in [:keyword_list, :non_empty_keyword_list, :map] ->
              {type, keys}

            other ->
              {other, _nested_schema = nil}
          end

        case validate_type(subtype, key, current_value) do
          {:ok, validated_value} when not is_nil(nested_schema) ->
            case validate_options_with_schema_and_path(
                   validated_value,
                   nested_schema,
                   _path = [key]
                 ) do
              {:ok, validated_value} ->
                {:cont, {validated_value, errors_acc}}

              {:error, %ValidationError{} = error} ->
                {:cont, {current_value, [error | errors_acc]}}
            end

          {:ok, validated_value} ->
            {:cont, {validated_value, errors_acc}}

          {:error, %ValidationError{} = reason} ->
            {:cont, {current_value, [reason | errors_acc]}}
        end
      end)

    case result do
      {value, []} ->
        {:ok, value}

      {_value, errors} ->
        message =
          "expected #{render_key(key)} to match all given types, but didn't match " <>
            "all of them. Here are the reasons why it didn't match each of the types:\n\n" <>
            Enum.map_join(errors, "\n", &("  * " <> Exception.message(&1)))

        error_tuple(key, value, message)
    end
  end

  defp validate_type({:or, subtypes}, key, value) do
    result =
      Enum.reduce_while(subtypes, _errors = [], fn subtype, errors_acc ->
        {subtype, nested_schema} =
          case subtype do
            {type, keys} when type in [:keyword_list, :non_empty_keyword_list, :map] ->
              {type, keys}

            other ->
              {other, _nested_schema = nil}
          end

        case validate_type(subtype, key, value) do
          {:ok, value} when not is_nil(nested_schema) ->
            case validate_options_with_schema_and_path(value, nested_schema, _path = [key]) do
              {:ok, value} -> {:halt, {:ok, value}}
              {:error, %ValidationError{} = error} -> {:cont, [error | errors_acc]}
            end

          {:ok, value} ->
            {:halt, {:ok, value}}

          {:error, %ValidationError{} = reason} ->
            {:cont, [reason | errors_acc]}
        end
      end)

    case result do
      {:ok, value} ->
        {:ok, value}

      errors when is_list(errors) ->
        message =
          "expected #{render_key(key)} to match at least one given type, but didn't match " <>
            "any. Here are the reasons why it didn't match each of the allowed types:\n\n" <>
            Enum.map_join(errors, "\n", &("  * " <> Exception.message(&1)))

        error_tuple(key, value, message)
    end
  end

  defp validate_type({:list, subtype}, key, value) when is_list(value) do
    {subtype, nested_schema} =
      case subtype do
        {type, keys} when type in [:keyword_list, :non_empty_keyword_list, :map] ->
          {type, keys}

        other ->
          {other, _nested_schema = nil}
      end

    updated_elements =
      for {elem, index} <- Stream.with_index(value) do
        case validate_type(subtype, {__MODULE__, :list, index}, elem) do
          {:ok, value} when not is_nil(nested_schema) ->
            case validate_options_with_schema_and_path(value, nested_schema, _path = [key]) do
              {:ok, updated_value} -> updated_value
              {:error, %ValidationError{} = error} -> throw({:error, index, error})
            end

          {:ok, updated_elem} ->
            updated_elem

          {:error, %ValidationError{} = error} ->
            throw({:error, error})
        end
      end

    {:ok, updated_elements}
  catch
    {:error, %ValidationError{} = error} ->
      error_tuple(key, value, "invalid list in #{render_key(key)}: #{error.message}")

    {:error, index, %ValidationError{} = error} ->
      error_tuple(
        key,
        value,
        "invalid list element at position #{index} in #{render_key(key)}: #{error.message}"
      )
  end

  defp validate_type({:list, _subtype}, key, value) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected list, got: #{inspect(value)}"
    )
  end

  defp validate_type({:tagged_tuple, tag, value_type}, key, {tag, value}) do
    with {:ok, value} <- validate_type(value_type, {__MODULE__, :tagged_tuple_value, key}, value) do
      {:ok, {tag, value}}
    end
  end

  defp validate_type({:tagged_tuple, _tag_type, _value_type}, key, value) do
    error_tuple(key, value, "expected tagged tuple in #{render_key(key)}, got: #{inspect(value)}")
  end

  defp validate_type({:spark_behaviour, _module}, _key, value)
       when is_atom(value) and not is_boolean(value) do
    {:ok, {value, []}}
  end

  defp validate_type({:spark_behaviour, _module}, _key, {module, opts})
       when is_atom(module) and not is_boolean(module) and is_list(opts) do
    {:ok, {module, opts}}
  end

  defp validate_type({:spark_behaviour, module}, key, value) do
    error_tuple(
      key,
      value,
      "expected module that adopted #{inspect(module)} behaviour, or a module and options in #{render_key(key)}, got: #{inspect(value)}"
    )
  end

  defp validate_type({:spark_behaviour, _module, _}, _key, value)
       when is_atom(value) and not is_boolean(value) do
    {:ok, {value, []}}
  end

  defp validate_type({:spark_behaviour, _module, _}, _key, {module, opts})
       when is_atom(module) and not is_boolean(module) do
    {:ok, {module, opts}}
  end

  defp validate_type({:spark_behaviour, module, _}, key, value) do
    error_tuple(
      key,
      value,
      "expected module that adopted #{inspect(module)} behaviour, or a module and options in #{render_key(key)}, got: #{inspect(value)}"
    )
  end

  defp validate_type({:protocol, protocol}, key, value) do
    protocol.impl_for!(value)

    {:ok, value}
  rescue
    Protocol.UndefinedError ->
      error_tuple(
        key,
        value,
        "expected a value which implements the `#{inspect(protocol)}` protocol"
      )

    UndefinedFunctionError ->
      error_tuple(key, value, "expected `#{inspect(protocol)}` to be a protocol")
  end

  defp validate_type({:impl, protocol}, key, value) do
    protocol.__info__(:module)
    Protocol.assert_protocol!(protocol)
    Protocol.assert_impl!(protocol, value)
    {:ok, value}
  rescue
    UndefinedFunctionError ->
      error_tuple(key, value, "#{inspect(protocol)} is not a protocol")

    e in ArgumentError ->
      message = Exception.message(e)

      cond do
        String.contains?(message, "reason :nofile") ->
          error_tuple(
            key,
            value,
            "protocol #{inspect(protocol)} is not implemented by #{inspect(value)}"
          )

        String.contains?(message, "not an atom") ->
          error_tuple(key, value, "expected module in #{render_key(key)}, got: #{inspect(value)}")

        true ->
          error_tuple(key, value, message)
      end
  end

  defp validate_type({type, _}, _key, value)
       when is_atom(value) and type in [:behaviour, :spark] do
    {:ok, value}
  end

  defp validate_type({type, _, _}, _key, value)
       when is_atom(value) and type in [:behaviour, :spark] do
    {:ok, value}
  end

  defp validate_type({type, _}, key, value) when type in [:behaviour, :spark] do
    error_tuple(key, value, "expected module in #{render_key(key)}, got: #{inspect(value)}")
  end

  defp validate_type({type, _, _}, key, value)
       when type in [:behaviour, :spark] do
    error_tuple(key, value, "expected module in #{render_key(key)}, got: #{inspect(value)}")
  end

  defp validate_type(:module, _key, value) when is_atom(value) do
    {:ok, value}
  end

  defp validate_type(:module, key, value) do
    error_tuple(key, value, "expected module in #{render_key(key)}, got: #{inspect(value)}")
  end

  defp validate_type({:spark_function_behaviour, _module, {_function_mod, _arity}}, key, value)
       when value in [nil, true, false] do
    error_tuple(key, nil, "expected module and opts, or a module in #{render_key(key)}, got: nil")
  end

  defp validate_type({:spark_function_behaviour, _module, {_function_mod, _arity}}, _key, value)
       when is_atom(value) do
    {:ok, {value, []}}
  end

  defp validate_type(
         {:spark_function_behaviour, _module, {_function_mod, _arity}},
         _key,
         {value, opts}
       )
       when is_atom(value) and is_list(opts) do
    {:ok, {value, opts}}
  end

  defp validate_type(
         {:spark_function_behaviour, _module, {function_mod, arity}},
         _key,
         value
       )
       when is_function(value, arity) do
    {:ok, {function_mod, fun: value}}
  end

  defp validate_type(
         {:spark_function_behaviour, _module, _module2, {_function_mod, _arity}},
         _key,
         nil
       ) do
    {:ok, nil}
  end

  defp validate_type(
         {:spark_function_behaviour, _module, _module2, {_function_mod, _arity}},
         _key,
         value
       )
       when is_atom(value) do
    {:ok, {value, []}}
  end

  defp validate_type(
         {:spark_function_behaviour, _module, _module2, {_function_mod, _arity}},
         _key,
         {value, opts}
       )
       when is_atom(value) and is_list(opts) do
    {:ok, {value, opts}}
  end

  defp validate_type(
         {:spark_function_behaviour, _module, _module2, {function_mod, arity}},
         _key,
         value
       )
       when is_function(value, arity) do
    {:ok, {function_mod, fun: value}}
  end

  defp validate_type({:spark_function_behaviour, _, {_, arity}}, key, value) do
    error_tuple(
      key,
      value,
      "expected module or function of arity #{inspect(arity)} in #{render_key(key)}, got: #{inspect(value)}"
    )
  end

  defp validate_type({:spark_function_behaviour, _, _, {_, arity}}, key, value) do
    error_tuple(
      key,
      value,
      "expected module or function of arity #{inspect(arity)} in #{render_key(key)}, got: #{inspect(value)}"
    )
  end

  defp validate_type({:mfa_or_fun, arity}, _key, value) when is_function(value, arity) do
    {:ok, value}
  end

  defp validate_type({:mfa_or_fun, _arity}, _key, {m, f, a} = value)
       when is_atom(m) and is_atom(f) and is_list(a) do
    {:ok, value}
  end

  defp validate_type({:mfa_or_fun, arity}, key, value) do
    error_tuple(
      key,
      value,
      "expected MFA or function of arity #{inspect(arity)} in #{render_key(key)}, got: #{inspect(value)}"
    )
  end

  defp validate_type({:spark_type, _, _}, _key, value) do
    {:ok, value}
  end

  defp validate_type({:spark_type, _, _, _}, _key, value) do
    {:ok, value}
  end

  defp validate_type(:struct, _key, value) when is_struct(value) do
    {:ok, value}
  end

  defp validate_type({:struct, mod}, _key, value) when is_struct(value, mod) do
    {:ok, value}
  end

  defp validate_type({:struct, mod}, key, value) do
    error_tuple(
      key,
      value,
      "expected instance of struct #{inspect(mod)} in #{render_key(key)}, got: #{inspect(value)}"
    )
  end

  defp validate_type({:wrap_list, type}, key, value) when not is_list(value) do
    validate_type({:wrap_list, type}, key, List.wrap(value))
  end

  defp validate_type({:wrap_list, type}, key, value) do
    validate_type({:list, type}, key, value)
  end

  defp validate_type(:quoted, _, value), do: {:ok, value}
  defp validate_type(:literal, _, value), do: {:ok, value}
  defp validate_type({:literal, value}, _, value), do: {:ok, value}

  defp validate_type({:literal, expected_value}, key, value) do
    error_tuple(
      key,
      value,
      "expected literal value #{inspect(expected_value)} #{render_key(key)}, got: #{inspect(value)}"
    )
  end

  defp validate_type({:tuple, tuple_def}, key, value)
       when is_tuple(value) and length(tuple_def) == tuple_size(value) do
    tuple_def
    |> Stream.with_index()
    |> Enum.reduce_while([], fn {subtype, index}, acc ->
      elem = elem(value, index)

      case validate_type(subtype, {__MODULE__, :tuple, index}, elem) do
        {:ok, updated_elem} -> {:cont, [updated_elem | acc]}
        {:error, %ValidationError{} = error} -> {:halt, error}
      end
    end)
    |> case do
      acc when is_list(acc) ->
        {:ok, acc |> Enum.reverse() |> List.to_tuple()}

      %ValidationError{} = error ->
        error_tuple(key, value, "invalid tuple in #{render_key(key)}: #{error.message}")
    end
  end

  defp validate_type({:tuple, tuple_def}, key, value) when is_tuple(value) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected tuple with #{length(tuple_def)} elements, got: #{inspect(value)}"
    )
  end

  defp validate_type({:tuple, _tuple_def}, key, value) do
    error_tuple(
      key,
      value,
      "invalid value for #{render_key(key)}: expected tuple, got: #{inspect(value)}"
    )
  end

  defp validate_type(_type, _key, value) do
    {:ok, value}
  end

  defp keyword_list?(value) do
    is_list(value) and Enum.all?(value, &match?({key, _value} when is_atom(key), &1))
  end

  defp available_types do
    types =
      Enum.map(@basic_types, &inspect/1) ++
        [
          "{:fun, arity}",
          "{:in, choices}",
          "{:and, subtypes}",
          "{:or, subtypes}",
          "{:custom, mod, fun, args}",
          "{:list, subtype}",
          "{:tuple, list_of_subtypes}",
          "{:map, key_type, value_type}",
          "{:struct, struct_name}"
        ]

    Enum.join(types, ", ")
  end

  @doc false
  def validate_type(value) when value in @basic_types do
    {:ok, value}
  end

  def validate_type({keyword_list, schema})
      when keyword_list in [:keyword_list, :non_empty_keyword_list] do
    with {:ok, validated_schema} <- validate_options_with_schema(schema, options_schema()) do
      {:ok, {keyword_list, validated_schema}}
    end
  end

  def validate_type({:wrap_list, type}) do
    with {:ok, type} <- validate_type(type) do
      {:ok, {:wrap_list, type}}
    end
  end

  def validate_type({:literal, term}), do: {:ok, {:literal, term}}

  def validate_type({:fun, arity} = value) when is_integer(arity) and arity >= 0 do
    {:ok, value}
  end

  def validate_type({:fun, list}) when is_list(list) do
    Enum.reduce_while(list, {:ok, list}, fn
      {type, _keys}, acc
      when type in [:keyword_list, :non_empty_keyword_list, :map] ->
        {:cont, acc}

      subtype, acc ->
        case validate_type(subtype) do
          {:ok, _value} -> {:cont, acc}
          {:error, reason} -> {:halt, {:error, "invalid type given to :fun type: #{reason}"}}
        end
    end)
  end

  def validate_type({:fun, list, returns}) when is_list(list) do
    Enum.reduce_while(list, {:ok, list}, fn
      {type, _keys}, acc
      when type in [:keyword_list, :non_empty_keyword_list, :map] ->
        {:cont, acc}

      subtype, acc ->
        case validate_type(subtype) do
          {:ok, _value} -> {:cont, acc}
          {:error, reason} -> {:halt, {:error, "invalid type given to :fun type: #{reason}"}}
        end
    end)
    |> case do
      {:ok, value} ->
        case validate_type(returns) do
          {:ok, returns} ->
            {:ok, {:fun, value, returns}}

          {:error, error} ->
            {:error, "invalid type given to :fun type: #{error}"}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  def validate_type({:spark_type, module, builtin_function} = type)
      when is_atom(module) and is_atom(builtin_function) do
    {:ok, type}
  end

  def validate_type({:spark_type, module, builtin_function, templates} = type)
      when is_atom(module) and is_atom(builtin_function) and is_list(templates) do
    {:ok, type}
  end

  def validate_type({:mfa_or_fun, integer}) when is_integer(integer) and integer >= 0 do
    {:ok, {:mfa_or_fun, integer}}
  end

  def validate_type(:struct) do
    {:ok, :struct}
  end

  def validate_type({:struct, module}) when is_atom(module) do
    {:ok, {:struct, module}}
  end

  def validate_type({:spark, module}) when is_atom(module) do
    {:ok, {:spark, module}}
  end

  def validate_type({:behaviour, module}) when is_atom(module) do
    {:ok, {:behaviour, module}}
  end

  def validate_type({:protocol, module}) when is_atom(module), do: {:ok, {:protocol, module}}

  def validate_type({:impl, module}) when is_atom(module), do: {:ok, {:impl, module}}

  def validate_type({:spark_behaviour, module}) when is_atom(module) do
    {:ok, {:spark_behaviour, module}}
  end

  def validate_type({:spark_behaviour, module1, module2} = type)
      when is_atom(module1) and is_atom(module2) do
    {:ok, type}
  end

  def validate_type({:spark_function_behaviour, module, {function_module, integer}} = type)
      when is_atom(module) and is_atom(function_module) and is_integer(integer) do
    {:ok, type}
  end

  def validate_type(
        {:spark_function_behaviour, module, module2, {function_module, integer}} = type
      )
      when is_atom(module) and is_atom(module2) and is_atom(function_module) and
             is_integer(integer) do
    {:ok, type}
  end

  def validate_type({:tagged_tuple, tag, inner_type}) when is_atom(tag) do
    case validate_type(inner_type) do
      {:ok, inner_type} ->
        {:ok, {:tagged_tuple, tag, inner_type}}

      {:error, error} ->
        {:error, "invalid type given to :tagged_tuple type: #{error}"}
    end
  end

  def validate_type({:tagged_tuple, tag, _inner_type}) do
    {:error, "invalid tag given to `:tagged_tuple`. Expected an atom, got: #{inspect(tag)}"}
  end

  # "choices" here can be any enumerable so there's no easy and fast way to validate it.
  def validate_type({in_container, _choices} = value) when in_container in [:one_of, :in] do
    {:ok, value}
  end

  def validate_type({:custom, mod, fun, args} = value)
      when is_atom(mod) and is_atom(fun) and is_list(args) do
    {:ok, value}
  end

  def validate_type({:or, subtypes} = value) when is_list(subtypes) do
    Enum.reduce_while(subtypes, {:ok, value}, fn
      {type, _keys}, acc
      when type in [:keyword_list, :non_empty_keyword_list, :map] ->
        {:cont, acc}

      subtype, acc ->
        case validate_type(subtype) do
          {:ok, _value} -> {:cont, acc}
          {:error, reason} -> {:halt, {:error, "invalid type given to :or type: #{reason}"}}
        end
    end)
  end

  def validate_type({:and, subtypes} = value) when is_list(subtypes) do
    Enum.reduce_while(subtypes, {:ok, value}, fn
      {type, _keys}, acc
      when type in [:keyword_list, :non_empty_keyword_list, :map] ->
        {:cont, acc}

      subtype, acc ->
        case validate_type(subtype) do
          {:ok, _value} -> {:cont, acc}
          {:error, reason} -> {:halt, {:error, "invalid type given to :and type: #{reason}"}}
        end
    end)
  end

  # This is to support the special-cased "{:list, {:keyword_list, my_key: [type: ...]}}",
  # like we do in the :or type.
  def validate_type({:list, {type, keys}})
      when type in [:keyword_list, :non_empty_keyword_list, :map] and is_list(keys) do
    {:ok, {:list, {type, keys}}}
  end

  def validate_type({:list, subtype}) do
    case validate_type(subtype) do
      {:ok, validated_subtype} -> {:ok, {:list, validated_subtype}}
      {:error, reason} -> {:error, "invalid subtype given to :list type: #{inspect(reason)}"}
    end
  end

  def validate_type({:tuple, tuple_def}) when is_list(tuple_def) do
    validated_def =
      Enum.map(tuple_def, fn subtype ->
        case validate_type(subtype) do
          {:ok, validated_subtype} ->
            validated_subtype

          {:error, reason} ->
            throw({:error, "invalid subtype given to :tuple type: #{inspect(reason)}"})
        end
      end)

    {:ok, {:tuple, validated_def}}
  catch
    {:error, reason} -> {:error, reason}
  end

  def validate_type({:map, key_type, value_type}) do
    valid_key_type =
      case validate_type(key_type) do
        {:ok, validated_key_type} -> validated_key_type
        {:error, reason} -> throw({:error, "invalid key_type for :map type: #{inspect(reason)}"})
      end

    valid_values_type =
      case validate_type(value_type) do
        {:ok, validated_values_type} ->
          validated_values_type

        {:error, reason} ->
          throw({:error, "invalid value_type for :map type: #{inspect(reason)}"})
      end

    {:ok, {:map, valid_key_type, valid_values_type}}
  catch
    {:error, reason} -> {:error, reason}
  end

  def validate_type({:struct, struct_name}) when is_atom(struct_name) do
    {:ok, {:struct, struct_name}}
  end

  def validate_type({:struct, struct_name}) do
    {:error, "invalid struct_name for :struct, expected atom, got #{inspect(struct_name)}"}
  end

  def validate_type(value) do
    {:error, "unknown type #{inspect(value)}.\n\nAvailable types: #{available_types()}"}
  end

  defp error_tuple(key, value, message) do
    {:error, %ValidationError{key: key, message: message, value: value}}
  end

  @doc false
  def render_key({__MODULE__, :key}), do: "map key"
  def render_key({__MODULE__, :value, key}), do: "map key #{inspect(key)}"
  def render_key({__MODULE__, :tuple, index}), do: "tuple element at position #{index}"
  def render_key({__MODULE__, :list, index}), do: "list element at position #{index}"
  def render_key({__MODULE__, :tagged_tuple_value, _key}), do: "tagged tuple elem 1"
  def render_key(key), do: inspect(key) <> " option"
end
