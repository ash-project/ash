defmodule Ash.Type.File.Implementation do
  @moduledoc """
  Behaviour for file implementations that are compatible with `Ash.Type.File`.
  """

  @typedoc "Any `module()` implementing the `Ash.Type.File.Implementation` behaviour."
  @type t() :: module()

  @typedoc "The source of the file the implementation operates on."
  @type source() :: term()

  @typedoc "Errors returned by the implementation."
  @type error() :: term()

  @doc """
  Return path of the file on disk.

  See: `Ash.Type.File.path/1`

  This callback is optional. If the file is not stored on disk, this callback
  can be omitted.
  """
  @callback path(file :: source()) :: {:ok, Path.t()} | {:error, error()}

  @doc """
  Open `IO.device()` for the file.

  See `Ash.Type.File.open/2`

  The return pid must point to a process following the
  [Erlang I/O Protocol](https://www.erlang.org/doc/apps/stdlib/io_protocol.html)
  like `StringIO` or `File`.
  """
  @callback open(file :: source(), modes :: [File.mode()]) ::
              {:ok, IO.device()} | {:error, error()}

  @optional_callbacks [path: 1]
end
