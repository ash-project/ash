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
      {:another_thing_with_no_description, nil}
    ]
end

defmodule StringEnum do
  @moduledoc false

  use Ash.Type.Enum, values: ["foo", "bar", "baz"]
end

defmodule MixedEnum do
  @moduledoc false

  use Ash.Type.Enum, values: [:foo, "bar", "baz"]
end
