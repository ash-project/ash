# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Status do
  @moduledoc false

  use Ash.Type.Enum, values: [:open, :Closed, :NeverHappened, :Always_Was]

  def match("never_happened"), do: {:ok, :NeverHappened}
  def match(value), do: super(value)
end

defmodule DescriptiveEnum do
  @moduledoc false

  use Ash.Type.Enum,
    values: [
      {:foo, "Clearly a foo"},
      {:bar, "Obviously a bar"},
      {:baz, "Undoubtedly a baz"},
      :a_thing_with_no_description,
      {:another_thing_with_no_description, nil},
      with_details: [description: "Look I have a description", label: "I have a label"]
    ]
end

defmodule StringEnum do
  @moduledoc false

  use Ash.Type.Enum, values: ["foo", "bar", "baz"]
end

defmodule MixedEnum do
  @moduledoc false

  use Ash.Type.Enum,
    values: [
      :foo,
      "bar",
      "baz",
      {"Foo", "Awesome Description"},
      {"StringWithDetails", [description: "Look I have a description", label: "Awesome Label"]},
      {:another_one, "Another Awesome Description"},
      with_details: [description: "Look I have a description", label: "I have a label"]
    ]
end
