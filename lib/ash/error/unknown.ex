defmodule Ash.Error.Unknown do
  @moduledoc "The top level unknown error container"
  use Ash.Error.Exception

  def_ash_error([:errors], class: :unknown)

  @type t :: %__MODULE__{}

  def exception(opts) do
    if opts[:error] do
      super(Keyword.update(opts, :errors, [opts[:error]], &[opts[:error] | &1]))
    else
      super(opts)
    end
  end

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "unknown"

    def message(%{errors: errors}) do
      Ash.Error.error_messages(errors, nil)
    end
  end
end
