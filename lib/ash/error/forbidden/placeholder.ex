defmodule Ash.Error.Forbidden.Placeholder do
  @moduledoc "Used when an api that has `require_actor? true` is provided no actor"
  use Ash.Error.Exception

  def_ash_error([:authorizer], class: :forbidden)

  def from_json(%{"authorizer" => authorizer}) do
    %__MODULE__{authorizer: Module.concat([authorizer])}
  end

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "placeholder"

    def message(%{authorizer: authorizer}) do
      "This is a placeholder error that should be replaced for authorizer `#{inspect(authorizer)}` automatically. If you get it, please report a bug."
    end
  end
end
