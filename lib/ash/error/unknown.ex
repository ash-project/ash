defmodule Ash.Error.Unknown do
  @moduledoc "The top level unknown error container"
  use Splode.ErrorClass, fields: [:changeset, :query, :action_input], class: :unknown

  @type t :: %__MODULE__{}

  def exception(opts) do
    if opts[:error] do
      super(Keyword.update(opts, :errors, [opts[:error]], &[opts[:error] | &1]))
    else
      super(opts)
    end
  end
end
