# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Calculations.CalculationWithUnionTypeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Changeset

  defmodule ChecklistContent do
    use Ash.Resource,
      data_layer: :embedded

    attributes do
      uuid_primary_key :id

      attribute :items, {:array, :map},
        public?: true,
        default: [],
        constraints: [
          items: [
            fields: [
              text: [type: :string, allow_nil?: false],
              completed: [type: :boolean]
            ]
          ]
        ]

      attribute :allow_reordering, :boolean, public?: true, default: true
      attribute :content_type, :string, public?: true, default: "checklist"
    end

    calculations do
      calculate :total_items, :integer, expr(length(items)) do
        public? true
      end

      calculate :completed_count, :integer, expr(0) do
        # In a real implementation, this would count completed items
        public? true
      end

      calculate :progress_percentage, :float, expr(0.0) do
        # In a real implementation, this would calculate percentage
        public? true
      end
    end
  end

  defmodule TextContent do
    use Ash.Resource,
      data_layer: :embedded

    attributes do
      uuid_primary_key :id

      attribute :text, :string, public?: true, allow_nil?: false

      attribute :formatting, :atom,
        public?: true,
        constraints: [one_of: [:plain, :markdown, :html]],
        default: :plain

      attribute :word_count, :integer, public?: true, default: 0
      attribute :content_type, :string, public?: true, default: "text"
    end

    calculations do
      calculate :display_text, :string, expr(text) do
        public? true
      end

      calculate :is_formatted, :boolean, expr(formatting != :plain) do
        public? true
      end
    end
  end

  defmodule Author do
    use Ash.Resource,
      domain: Ash.Test.Calculations.CalculationWithUnionTypeTest.Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:create, :read]
    end

    attributes do
      uuid_primary_key :id, writable?: true

      attribute :content, :union do
        public? true

        constraints types: [
                      text: [
                        type: TextContent,
                        tag: :content_type,
                        tag_value: "text"
                      ],
                      # Simple types for testing untagged unions
                      note: [
                        type: :string
                      ]
                    ],
                    storage: :type_and_value
      end
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

    resources do
      resource Author
    end
  end

  test "load statement for calculations on embedded resource in union type doesn't fail when value is primitive" do
    assert %Author{content: content} =
             Changeset.for_create(Author, :create, %{content: "Just a note"})
             |> Ash.create!()
             |> Ash.load!(content: [text: [:is_formatted], checklist: [:total_items]])

    assert content == %Ash.Union{type: :note, value: "Just a note"}
  end

  test "load statement for calculations on embedded resource in union type can be loaded when value is an embedded resource" do
    assert %Author{content: content} =
             Changeset.for_create(Author, :create, %{
               content: %{text: "Text content", content_type: "text"}
             })
             |> Ash.create!()
             |> Ash.load!(content: [text: [:is_formatted], checklist: [:total_items]])

    assert %Ash.Union{type: :text, value: %{content_type: "text", text: "Text content"}} = content
  end
end
