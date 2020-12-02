defmodule Ash.Resource.Actions.Argument do
  @moduledoc "Represents an argument to an action"
  defstruct [:allow_nil?, :type, :name, constraints: []]

  @type t :: %__MODULE__{}

  def schema do
    [
      allow_nil?: [
        type: :boolean,
        default: true
      ],
      type: [
        type: {:custom, Ash.OptionsHelpers, :ash_type, []},
        required: true
      ],
      name: [
        type: :atom,
        required: true
      ],
      constraints: [
        type: :keyword_list,
        default: []
      ]
    ]
  end
end
