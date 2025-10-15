# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.TypedStruct do
  @moduledoc """
  A DSL for defining typed structs with field validation and constraints.

  `Ash.TypedStruct` provides a convenient way to define a struct type in Ash.

  Under the hood, it creates an `Ash.Type.NewType` with `subtype_of: :struct`
  and the appropriate constraints.

  ## Example

      defmodule MyApp.UserProfile do
        use Ash.TypedStruct

        typed_struct do
          field :username, :string, allow_nil?: false
          field :email, :string, constraints: [match: ~r/@/]
          field :age, :integer, constraints: [min: 0, max: 150]
          field :bio, :string, default: ""
          field :verified, :boolean, default: false
        end
      end

      # Creating instances
      {:ok, profile} = MyApp.UserProfile.new(username: "john", email: "john@example.com")

      # Using new! for raising on errors
      profile = MyApp.UserProfile.new!(username: "jane", email: "jane@example.com", age: 25)

      # Can be used as an Ash type
      defmodule MyApp.User do
        use Ash.Resource

        attributes do
          attribute :profile, MyApp.UserProfile
        end
      end

  ## Field Options

  * `:type` - The Ash type of the field (required)
  * `:default` - Default value for the field
  * `:allow_nil?` - Whether the field can be nil (defaults to `true`)
  * `:constraints` - Type-specific constraints (e.g., `:min`, `:max`, `:match`)
  * `:description` - Field documentation

  ## Constructor Functions

  The generated module includes:
  * `new/1` - Returns `{:ok, struct}` or `{:error, error}`
  * `new!/1` - Returns the struct or raises an error

  ## Overriding new/1

  You can override the `new/1` function to add custom logic:

      defmodule MyApp.CustomStruct do
        use Ash.TypedStruct

        typed_struct do
          field :name, :string, allow_nil?: false
          field :created_at, :utc_datetime
        end

        def new(params) do
          params = Map.put_new(params, :created_at, DateTime.utc_now())
          super(params)
        end
      end
  """

  defmodule Field do
    @moduledoc "Represents a field on a typed struct"
    defstruct [
      :name,
      :type,
      :constraints,
      :default,
      :allow_nil?,
      :description,
      :__spark_metadata__
    ]

    @type t :: %__MODULE__{
            name: atom(),
            constraints: Keyword.t(),
            type: Ash.Type.t(),
            default: nil | term | (-> term),
            __spark_metadata__: Spark.Dsl.Entity.spark_meta()
          }
  end

  defmodule Dsl do
    @moduledoc false
    @field %Spark.Dsl.Entity{
      name: :field,
      target: Field,
      describe: "A field on the struct",
      args: [:name, :type],
      transform: {Ash.Type, :set_type_transformation, []},
      schema: [
        name: [
          type: :atom,
          required: true,
          doc: "The name of the struct field"
        ],
        type: [
          type: Ash.OptionsHelpers.ash_type(),
          required: true,
          doc: "The type of the struct field"
        ],
        default: [
          type: :any,
          doc: "the default value for the field"
        ],
        description: [
          type: :any,
          doc: "a description for the field"
        ],
        constraints: [
          type: :any,
          default: [],
          doc: """
          Constraints to provide to the type when casting the value. For more, see `Ash.Type`.
          """
        ],
        allow_nil?: [
          type: :boolean,
          default: true,
          doc: """
          Whether or not the field can be set to nil.
          """
        ]
      ]
    }

    @struct %Spark.Dsl.Section{
      name: :typed_struct,
      describe: """
      Describe the fields of the struct
      """,
      after_define: {__MODULE__, :after_define},
      entities: [
        @field
      ]
    }

    # sobelow_skip ["RCE.CodeModule"]
    def after_define do
      quote do
        fields = Spark.Dsl.Extension.get_entities(__MODULE__, [:typed_struct])
        fields_with_defaults = Enum.map(fields, &{&1.name, &1.default})
        module = __MODULE__

        defaults =
          Map.new(Enum.reject(fields_with_defaults, &is_nil(elem(&1, 1))))

        enforce_keys =
          Enum.flat_map(fields, fn field ->
            if field.allow_nil? == false and is_nil(field.default) do
              [field.name]
            else
              []
            end
          end)

        # map_required_fields_match =
        #   enforce_keys
        #   |> Enum.reject(&(&1 in Map.keys(defaults)))
        #   |> Enum.map(&{&1, {:_, [], Elixir}})
        #   |> then(&{:%{}, [], &1})

        map_constraints =
          [
            fields:
              Keyword.new(fields, fn field ->
                {field.name,
                 Keyword.new(
                   Map.take(field, [
                     :type,
                     :constraints,
                     :description,
                     :constraints,
                     :allow_nil?
                   ])
                 )
                 |> Enum.reject(&is_nil(elem(&1, 1)))}
              end),
            instance_of: module
          ]

        # sobelow_skip ["RCE.CodeModule"]
        Code.eval_quoted(
          Ash.TypedStruct.__define_typed_struct__(
            enforce_keys,
            fields_with_defaults,
            map_constraints,
            defaults
            # map_required_fields_match
          ),
          [],
          __ENV__
        )
      end
    end

    use Spark.Dsl.Extension,
      sections: [@struct]
  end

  @doc false
  def __define_typed_struct__(
        enforce_keys,
        fields_with_defaults,
        map_constraints,
        defaults
        # map_required_fields_match
      ) do
    keyed_defaults =
      Enum.reduce(defaults, %{}, fn {k, v}, acc ->
        Map.put(acc, k, {to_string(k), v})
      end)

    quote do
      @enforce_keys unquote(enforce_keys)
      defstruct unquote(Macro.escape(fields_with_defaults))

      use Ash.Type.NewType,
        subtype_of: :struct,
        constraints: unquote(Macro.escape(map_constraints))

      # if Enum.any?(unquote(enforce_keys), &(&1 not in Map.keys(unquote(Macro.escape(defaults))))) do
      #   @doc "Create a new #{__MODULE__}, raising any error"
      #   def new!(unquote(map_required_fields_match) = fields) do
      #     fields
      #     |> new()
      #     |> Ash.Helpers.unwrap_or_raise!()
      #   end
      # end

      def new!(fields) do
        fields
        |> new()
        |> Ash.Helpers.unwrap_or_raise!()
      end

      @doc "Create a new #{__MODULE__}, or return an error"
      def new(%__MODULE__{} = fields) do
        fields = Map.merge(unquote(Macro.escape(defaults)), fields)

        case do_constraints(fields, []) do
          {:ok, value} -> {:ok, value}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def new(%_{} = fields) do
        fields =
          Enum.reduce(unquote(Macro.escape(keyed_defaults)), fields, fn {key, {string_key, value}},
                                                                        acc ->
            if Map.has_key?(acc, key) || Map.has_key?(acc, string_key) do
              acc
            else
              Map.put(acc, key, value)
            end
          end)

        case do_constraints(fields, unquote(Macro.escape(map_constraints))) do
          {:ok, value} -> {:ok, value}
          {:error, error} -> {:error, Ash.Error.to_ash_error(error)}
        end
      end

      def new(fields) do
        fields =
          Enum.reduce(unquote(Macro.escape(keyed_defaults)), Map.new(fields), fn {key,
                                                                                  {string_key,
                                                                                   value}},
                                                                                 acc ->
            if Map.has_key?(acc, key) || Map.has_key?(acc, string_key) do
              acc
            else
              Map.put(acc, key, value)
            end
          end)

        case do_constraints(
               fields,
               unquote(Macro.escape(map_constraints))
             ) do
          {:ok, value} ->
            {:ok, value}

          {:error, error} ->
            Ash.Type.CompositeTypeHelpers.convert_errors_to_invalid_attributes(error)
        end
      end

      def cast_input("", _), do: {:ok, nil}

      def cast_input(nil, _), do: {:ok, nil}

      def cast_input(value, constraints) when is_binary(value) do
        case Ash.Helpers.json_module().decode(value) do
          {:ok, value} ->
            cast_input(value, constraints)

          _ ->
            :error
        end
      end

      def cast_input(v, constraints) do
        with {:ok, v} <- new(v) do
          super(v, constraints)
        end
      end

      defp do_constraints(value, constraints) when is_map(value) do
        Ash.Type.apply_constraints(Ash.Type.Struct, value, constraints)
      end

      defoverridable new: 1
    end
  end

  use Spark.Dsl, default_extensions: [extensions: [Dsl]]
end
