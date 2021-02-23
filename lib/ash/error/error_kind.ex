defprotocol Ash.ErrorKind do
  @moduledoc false

  @spec id(t()) :: String.t()
  def id(error)

  @spec code(t()) :: String.t()
  def code(error)

  @spec message(t()) :: String.t()
  def message(error)
end
