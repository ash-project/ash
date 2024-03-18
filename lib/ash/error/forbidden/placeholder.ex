defmodule Ash.Error.Forbidden.Placeholder do
  @moduledoc "A placeholder exception that the user should never see"
  use Ash.Error.Exception

  use Splode.Error, fields: [:authorizer], class: :forbidden

  def from_json(%{"authorizer" => authorizer}) do
    %__MODULE__{authorizer: Module.concat([authorizer])}
  end

  def message(%{authorizer: authorizer}) do
    "This is a placeholder error that should be replaced for authorizer `#{inspect(authorizer)}` automatically. If you get it, please report a bug."
  end
end
