defmodule Ash.Type.File do
  @moduledoc """
  A type that represents a file on the filesystem.

  > #### Persistence {: .warning}
  >
  > This type does not support persisting via `Ash.DataLayer`.
  >
  > It is mainly intended to be used in
  > [arguments](dsl-ash-resource.html#actions-action-argument).

  ## Valid values to cast

  This type can cast multiple types of values:

  * itself
  * `Plug.Upload`
  * Any value that implements the `Ash.Type.File.Source` protocol.
  """

  use Ash.Type

  alias Ash.Type.File.Implementation
  alias Ash.Type.File.IO, as: IOImplementation
  alias Ash.Type.File.Path, as: PathImplementation
  alias Ash.Type.File.Source

  @enforce_keys [:source, :implementation]
  defstruct [:source, :implementation]
  @type t() :: %__MODULE__{source: Implementation.source(), implementation: Implementation.t()}

  @impl Ash.Type
  def storage_type(_constraints), do: :error

  @impl Ash.Type
  def cast_input(nil, _constraints), do: {:ok, nil}
  def cast_input(%__MODULE__{} = file, _constraints), do: {:ok, file}

  def cast_input(source, _constraints) do
    with {:ok, implementation} <- Source.implementation(source) do
      {:ok, %__MODULE__{source: source, implementation: implementation}}
    end
  end

  @impl Ash.Type
  def cast_stored(_file, _constraints), do: :error

  @impl Ash.Type
  def dump_to_native(_file, _constraints), do: :error

  @doc """
  Returns the path to the file.

  Not every implementation will support this operation. If the implementation
  does not support this operation, then `{:error, :not_supported}` will be
  returned. In this case, use the `open/2` function to access the file.

  ## Example

      iex> path = "README.md"
      ...> file = Ash.Type.File.from_path(path)
      ...> Ash.Type.File.path(file)
      {:ok, "README.md"}

  """
  @spec path(file :: t()) :: {:ok, Path.t()} | {:error, :not_supported | Implementation.error()}
  def path(%__MODULE__{implementation: implementation, source: source}) do
    Code.ensure_loaded!(implementation)

    if function_exported?(implementation, :path, 1) do
      implementation.path(source)
    else
      {:error, :not_supported}
    end
  end

  @doc """
  Open the file with the given `modes`.

  This function will delegate to the `open/2` function on the `implementation`.

  For details on the `modes` argument, see the `File.open/2` documentation.

  ## Example

      iex> path = "README.md"
      ...> file = Ash.Type.File.from_path(path)
      ...> Ash.Type.File.open(file, [:read])
      ...> # => {:ok, #PID<0.109.0>}

  """
  @spec open(file :: t(), modes :: [File.mode()]) ::
          {:ok, IO.device()} | {:error, Implementation.error()}
  def open(%__MODULE__{implementation: implementation, source: source}, modes \\ []),
    do: implementation.open(source, modes)

  @doc """
  Create a file from a path.

  ## Example

      iex> path = "README.md"
      ...> Ash.Type.File.from_path(path)
      %Ash.Type.File{source: "README.md", implementation: Ash.Type.File.Path}

  """
  @spec from_path(path :: Path.t()) :: t()
  def from_path(path), do: %__MODULE__{source: path, implementation: PathImplementation}

  @doc """
  Create a file from an `IO.device()`

  ## Example

      iex> path = "README.md"
      ...> {:ok, device} = File.open(path)
      ...> Ash.Type.File.from_io(device)
      %Ash.Type.File{source: device, implementation: Ash.Type.File.IO}

  """
  @spec from_io(device :: IO.device()) :: t()
  def from_io(device), do: %__MODULE__{source: device, implementation: IOImplementation}
end
