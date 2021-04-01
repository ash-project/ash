defmodule Mix.Tasks.Ash.TemplatesTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  alias Mix.Tasks.Ash.Templates

  test "IO messages" do
    assert cio_print_info([{:json_api, true}]) =~ "{:ash_json_api"
    assert cio_print_info([{:json_api, true}]) =~ "extensions: [AshJsonApi.Api]"
    assert cio_print_info([{:json_api, true}]) =~ "json_api do\n"
    assert cio_print_info([{:graphql, true}]) =~ "{:ash_graphql"
    assert cio_print_info([{:graphql, true}]) =~ "graphql do\n"
    assert cio_print_info([{:postgres, true}]) =~ "{:ash_postgres"
    assert cio_print_info([{:csv, true}]) =~ "{:ash_csv"
    assert cio_print_info([{:phoenix, true}]) =~ "{:ash_phoenix"
    assert cio_print_info([{:policy_authorizer, true}]) =~ "{:ash_policy_authorizer"
  end

  def cio_print_info(switches) do
    capture_io(fn -> Templates.print_info(switches) end)
  end
end
