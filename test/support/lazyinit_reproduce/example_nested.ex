defmodule Type.LazyInitTest.ExampleNested do
  @moduledoc false
  use Ash.Type.NewType, subtype_of: Type.LazyInitTest.Example, lazy_init?: true
end
