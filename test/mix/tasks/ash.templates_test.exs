defmodule Mix.Tasks.Ash.TemplateTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  alias Ash.Template

  test "IO messages" do
    assert cio_print_required_changes([{:json_api, true}]) =~ "extensions: [AshJsonApi.Api]"
    assert cio_print_required_changes([{:json_api, true}]) =~ "json_api do\n"
    assert cio_print_required_changes([{:graphql, true}]) =~ "graphql do\n"

    assert cio_print_dependencies([{:csv, true}]) =~ "{:ash_csv"
    assert cio_print_dependencies([{:graphql, true}]) =~ "{:ash_graphql"
    assert cio_print_dependencies([{:json_api, true}]) =~ "{:ash_json_api"
    assert cio_print_dependencies([{:phoenix, true}]) =~ "{:ash_phoenix"
    assert cio_print_dependencies([{:policy_authorizer, true}]) =~ "{:ash_policy_authorizer"
    assert cio_print_dependencies([{:postgres, true}]) =~ "{:ash_postgres"
  end

  def cio_print_required_changes(switches) do
    capture_io(fn -> Template.print_required_changes(switches) end)
  end

  def cio_print_dependencies(switches) do
    capture_io(fn -> Template.print_dependencies(switches) end)
  end
end
