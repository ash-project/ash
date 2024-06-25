defmodule Ash.Resource.Attribute do
  @moduledoc "Represents an attribute on a resource"

  defstruct [
    :name,
    :type,
    :allow_nil?,
    :generated?,
    :primary_key?,
    :public?,
    :writable?,
    :always_select?,
    :default,
    :update_default,
    :description,
    :source,
    match_other_defaults?: false,
    sensitive?: false,
    filterable?: true,
    sortable?: true,
    constraints: []
  ]

  defmodule Helpers do
    @moduledoc "Helpers for building attributes"

    defmacro timestamps(opts \\ []) do
      quote do
        create_timestamp :inserted_at, unquote(opts)
        update_timestamp :updated_at, unquote(opts)
      end
    end
  end

  @type t :: %__MODULE__{
          name: atom(),
          constraints: Keyword.t(),
          type: Ash.Type.t(),
          primary_key?: boolean(),
          public?: boolean(),
          sortable?: boolean(),
          default: nil | term | (-> term),
          update_default: nil | term | (-> term) | (Ash.Resource.record() -> term),
          sensitive?: boolean(),
          writable?: boolean()
        }

  @schema [
    name: [
      type: :atom,
      doc: "The name of the attribute.",
      required: true
    ],
    type: [
      type: Ash.OptionsHelpers.ash_type(),
      doc: "The type of the attribute. See `Ash.Type` for more.",
      required: true
    ],
    constraints: [
      type: :keyword_list,
      doc: """
      Constraints to provide to the type when casting the value. For more, see `Ash.Type`.
      """
    ],
    description: [
      type: :string,
      doc: "An optional description for the attribute."
    ],
    sensitive?: [
      type: :boolean,
      default: false,
      doc: """
      Whether or not the attribute value contains sensitive information, like PII. See the [Sensitive Data guide](/documentation/topics/security/sensitive-data.md) for more.
      """
    ],
    source: [
      type: :atom,
      doc: """
      If the field should be mapped to a different name in the data layer. Support varies by data layer.
      """
    ],
    always_select?: [
      type: :boolean,
      default: false,
      doc: """
      Whether or not to ensure this attribute is always selected when reading from the database, regardless of applied select statements.
      """
    ],
    primary_key?: [
      type: :boolean,
      default: false,
      doc: """
      Whether the attribute is the primary key. Composite primary key is also possible by using `primary_key? true` in more than one attribute. If primary_key? is true, allow_nil? must be false.
      """
    ],
    allow_nil?: [
      type: :boolean,
      default: true,
      doc: """
      Whether or not the attribute can be set to nil. If nil value is given error is raised.
      """
    ],
    generated?: [
      type: :boolean,
      default: false,
      doc: """
      Whether or not the value may be generated by the data layer.
      """
    ],
    writable?: [
      type: :boolean,
      default: true,
      doc: """
      Whether or not the value can be written to. Non-writable attributes can still be written with `Ash.Changeset.force_change_attribute/3`.
      """
    ],
    public?: [
      type: :boolean,
      default: false,
      doc: """
      Whether or not the attribute should be shown over public interfaces. See the [sensitive data guide](/documentation/topics/security/sensitive-data.md) for more.
      """
    ],
    default: [
      type: {:or, [{:mfa_or_fun, 0}, :literal]},
      doc: """
      A value to be set on all creates, unless a value is being provided already.  Note: The default value is casted according to the type's Ash.Type.* module, before it is saved.  For `:string`, for example, if `constraints: [allow_empty?: _]` is false, the value `\"\"` will be cast to `nil`.  See the `:constraints` option, the `:allow_nil?` option, and the relevant `Ash.Type.*` documentation.
      """
    ],
    update_default: [
      type: {:or, [{:mfa_or_fun, 0}, :literal]},
      doc: "A value to be set on all updates, unless a value is being provided already."
    ],
    filterable?: [
      type: {:or, [:boolean, {:in, [:simple_equality]}]},
      default: true,
      doc: """
      Whether or not the attribute can be referenced in filters.
      """
    ],
    sortable?: [
      default: true,
      type: :boolean,
      doc: """
      Whether or not the attribute can be referenced in sorts.
      """
    ],
    match_other_defaults?: [
      type: :boolean,
      default: false,
      doc: """
      Ensures that other attributes that use the same "lazy" default (a function or an mfa), use the same default value. Has no effect unless `default` is a zero argument function.
      """
    ]
  ]

  @create_timestamp_schema @schema
                           |> Spark.Options.Helpers.set_default!(:writable?, false)
                           |> Spark.Options.Helpers.set_default!(:default, &DateTime.utc_now/0)
                           |> Spark.Options.Helpers.set_default!(:match_other_defaults?, true)
                           |> Spark.Options.Helpers.set_default!(:type, Ash.Type.UtcDatetimeUsec)
                           |> Spark.Options.Helpers.set_default!(:allow_nil?, false)
                           |> Ash.OptionsHelpers.hide_all_except([:name])

  @update_timestamp_schema @schema
                           |> Spark.Options.Helpers.set_default!(:writable?, false)
                           |> Spark.Options.Helpers.set_default!(:match_other_defaults?, true)
                           |> Spark.Options.Helpers.set_default!(:default, &DateTime.utc_now/0)
                           |> Spark.Options.Helpers.set_default!(
                             :update_default,
                             &DateTime.utc_now/0
                           )
                           |> Spark.Options.Helpers.set_default!(:type, Ash.Type.UtcDatetimeUsec)
                           |> Spark.Options.Helpers.set_default!(:allow_nil?, false)
                           |> Ash.OptionsHelpers.hide_all_except([:name])

  @uuid_primary_key_schema @schema
                           |> Spark.Options.Helpers.set_default!(:public?, true)
                           |> Spark.Options.Helpers.set_default!(:writable?, false)
                           |> Spark.Options.Helpers.set_default!(:default, &Ash.UUID.generate/0)
                           |> Spark.Options.Helpers.set_default!(:primary_key?, true)
                           |> Spark.Options.Helpers.set_default!(:type, :uuid)
                           |> Keyword.delete(:allow_nil?)
                           |> Ash.OptionsHelpers.hide_all_except([:name])

  @uuid_v7_primary_key_schema @schema
                              |> Spark.Options.Helpers.set_default!(:public?, true)
                              |> Spark.Options.Helpers.set_default!(:writable?, false)
                              |> Spark.Options.Helpers.set_default!(
                                :default,
                                &Ash.UUIDv7.generate/0
                              )
                              |> Spark.Options.Helpers.set_default!(:primary_key?, true)
                              |> Spark.Options.Helpers.set_default!(:type, :uuid_v7)
                              |> Keyword.delete(:allow_nil?)
                              |> Ash.OptionsHelpers.hide_all_except([:name])

  @integer_primary_key_schema @schema
                              |> Spark.Options.Helpers.set_default!(:public?, true)
                              |> Spark.Options.Helpers.set_default!(:writable?, false)
                              |> Spark.Options.Helpers.set_default!(:primary_key?, true)
                              |> Spark.Options.Helpers.set_default!(:generated?, true)
                              |> Spark.Options.Helpers.set_default!(:type, :integer)
                              |> Keyword.delete(:allow_nil?)
                              |> Ash.OptionsHelpers.hide_all_except([:name])

  def transform(attribute) do
    Ash.Type.set_type_transformation(%{attribute | source: attribute.source || attribute.name})
  end

  @doc false
  def attribute_schema, do: @schema
  def create_timestamp_schema, do: @create_timestamp_schema
  def update_timestamp_schema, do: @update_timestamp_schema
  def uuid_primary_key_schema, do: @uuid_primary_key_schema
  def uuid_v7_primary_key_schema, do: @uuid_v7_primary_key_schema
  def integer_primary_key_schema, do: @integer_primary_key_schema
end
