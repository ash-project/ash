defmodule Ash.Page do
  @moduledoc "Types for Ash pages"
  @type page :: Ash.Page.Keyset.t() | Ash.Page.Offset.t()
end
