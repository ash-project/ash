defmodule Ash.Authorization.Check do
  @moduledoc """
  A behaviour for declaring checks, which can be used to easily construct
  authorization rules.
  """

  @type options :: Keyword.t()

  @callback strict_check(Ash.user(), Ash.Authorization.request(), options) :: boolean | :unknown
  @callback prepare(options) ::
              list(Ash.Authorization.prepare_instruction()) | {:error, Ash.error()}
  @callback check(Ash.user(), list(Ash.record()), map, options) ::
              {:ok, list(Ash.record()) | boolean} | {:error, Ash.error()}
  @callback describe(options()) :: String.t()
  @callback action_types() :: list(Ash.action_type())

  @optional_callbacks check: 4, prepare: 1

  def defines_check?(module) do
    :erlang.function_exported(module, :check, 4)
  end

  defmacro __using__(opts) do
    quote do
      @behaviour Ash.Authorization.Check

      @impl true
      def prepare(_), do: []

      @impl true
      def action_types(), do: unquote(opts[:action_types])

      defoverridable prepare: 1, action_types: 0
    end
  end

  defmacro import_default_checks(opts) do
    quote do
      import Ash.Authorization.Check.Static, only: [always: 0, never: 0]
      import Ash.Authorization.Check.RelatedToUserVia, only: [related_to_user_via: 1]
      import Ash.Authorization.Check.SettingAttribute, only: [setting_attribute: 2]

      import Ash.Authorization.Check.UserAttributeMatchesRecord,
        only: [user_attribute_matches_record: 2]

      import Ash.Authorization.Check.UserAttribute, only: [user_attribute: 2]

      if unquote(opts[:attributes]) do
        import Ash.Authorization.Check.SettingAttribute,
          only: [setting_attribute: 2, setting_attribute: 1]
      else
        import Ash.Authorization.Check.AttributeEquals, only: [attribute_equals: 2]
      end
    end
  end

  defmacro unimport_checks() do
    quote do
      import Ash.Authorization.Check.Static, only: []
      import Ash.Authorization.Check.RelatedToUserVia, only: []
      import Ash.Authorization.Check.SettingAttribute, only: []
      import Ash.Authorization.Check.UserAttributeMatchesRecord, only: []
      import Ash.Authorization.Check.UserAttribute, only: []
      import Ash.Authorization.Check.SettingAttribute, only: []
    end
  end
end
