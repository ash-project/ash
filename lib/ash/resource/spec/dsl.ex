defmodule Ash.Resource.Spec.Dsl do
  @moduledoc """
  DSL extension for defining resource specifications.
  """

  # Use the same entities as the main resource DSL
  @attribute %Spark.Dsl.Entity{
    name: :attribute,
    describe: "Declares a required attribute that implementing resources must define.",
    examples: [
      """
      attribute :name, :string do
        allow_nil? false
      end
      """
    ],
    transform: {Ash.Resource.Attribute, :transform, []},
    target: Ash.Resource.Attribute,
    args: [:name, :type],
    schema: Ash.Resource.Attribute.attribute_schema()
  }

  @create_timestamp %Spark.Dsl.Entity{
    name: :create_timestamp,
    describe: "Declares a required create timestamp attribute.",
    examples: [
      "create_timestamp :inserted_at"
    ],
    target: Ash.Resource.Attribute,
    args: [:name],
    schema: Ash.Resource.Attribute.create_timestamp_schema(),
    transform: {Ash.Resource.Attribute, :transform, []}
  }

  @update_timestamp %Spark.Dsl.Entity{
    name: :update_timestamp,
    describe: "Declares a required update timestamp attribute.",
    examples: [
      "update_timestamp :updated_at"
    ],
    target: Ash.Resource.Attribute,
    schema: Ash.Resource.Attribute.update_timestamp_schema(),
    args: [:name],
    transform: {Ash.Resource.Attribute, :transform, []}
  }

  @sections [
    %Spark.Dsl.Section{
      name: :attributes,
      describe: """
      Required attributes that implementing resources must define.
      """,
      entities: [
        @attribute,
        @create_timestamp,
        @update_timestamp
      ]
    }
  ]

  use Spark.Dsl.Extension,
    sections: @sections
end
