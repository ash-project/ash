# This file is only used by Telemetry as a dependency.
# Use rebar3 instead for compiling, running tests, etc.
defmodule Telemetry.MixProject do
  use Mix.Project

  {:ok, [{:application, :telemetry, props}]} = :file.consult("src/telemetry.app.src")
  @props Keyword.take(props, [:applications, :description, :env, :mod, :vsn])

  def application do
    @props
  end

  def project do
    [
      app: :telemetry,
      version: to_string(application()[:vsn]),
      language: :erlang
    ]
  end
end
