defmodule Ash.Error.Forbidden do
  @moduledoc "Raised when authorization for an action fails"

  use Ash.Error

  def_ash_error(
    [
      :errors,
      :scenarios,
      :requests,
      :facts,
      :state,
      :strict_access?,
      :api,
      verbose?: false,
      no_steps_configured: false
    ],
    class: :forbidden
  )

  defimpl Ash.ErrorKind do
    alias Ash.Authorization.Report

    def id(_), do: Ecto.UUID.generate()

    def message(%{errors: errors}) when not is_nil(errors) do
      Ash.Error.error_messages(errors)
    end

    def message(error) do
      if error.verbose? do
        description(error)
      else
        "forbidden"
      end
    end

    def code(_), do: "Forbidden"

    def description(%{errors: errors}) when not is_nil(errors) do
      Ash.Error.error_descriptions(errors)
    end

    def description(error) do
      report = %Report{
        api: error.api,
        scenarios: error.scenarios,
        requests: error.requests,
        facts: error.facts,
        state: error.state,
        no_steps_configured: error.no_steps_configured,
        header: "forbidden:",
        authorized?: false
      }

      Report.report(report)
    end
  end
end
