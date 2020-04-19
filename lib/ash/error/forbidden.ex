defmodule Ash.Error.Forbidden do
  @moduledoc "Raised when authorization for an action fails"

  alias Ash.Authorization.Report

  defexception [
    :scenarios,
    :requests,
    :facts,
    :state,
    :strict_access?,
    :reason,
    path: [],
    no_steps_configured: false
  ]

  def message(error) do
    report = %Report{
      path: error.path,
      reason: error.reason,
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

  def report_text(error, header \\ "forbidden:") do
    report = %Report{
      path: error.path,
      reason: error.reason,
      scenarios: error.scenarios,
      requests: error.requests,
      facts: error.facts,
      state: error.state,
      no_steps_configured: error.no_steps_configured,
      header: header,
      authorized?: false
    }

    Report.report(report)
  end
end
