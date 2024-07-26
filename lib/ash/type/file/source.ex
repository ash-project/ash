defprotocol Ash.Type.File.Source do
  @moduledoc """
  Protocol for allowing the casting of something into an `Ash.Type.File`.

  ## Usage

  ```elixir
  defmodule MyStruct do
    defstruct [:path]

    @behavior Ash.Type.File.Implementation

    @impl Ash.Type.File.Implementation
    def path(%__MODULE__{path: path}), do: {:ok, path}

    @impl Ash.Type.File.Implementation
    def open(%__MODULE__{path: path}, modes), do: File.open(path, modes)

    defimpl Ash.Type.File.Source do
      def implementation(%MyStruct{} = struct), do: {:ok, MyStruct}
    end
  end
  ```
  """

  @fallback_to_any true

  alias Ash.Type.File.Implementation

  @doc """
  Detect Implementation of the file.

  Returns an `:ok` tuple with the implementation module if the file is supported
  and `:error` otherwise.
  """
  @spec implementation(t()) :: {:ok, Implementation.t()} | :error
  def implementation(file)
end

defimpl Ash.Type.File.Source, for: Any do
  def implementation(_file), do: :error
end
