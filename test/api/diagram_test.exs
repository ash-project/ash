defmodule Ash.Test.Api.Info.DiagramTest do
  @moduledoc false
  use ExUnit.Case, async: true

  test "generate a mermaid entity relationship diagram from an Api" do
    assert Ash.Api.Info.Diagram.mermaid(Ash.Test.Support.Flow.Api) == """
           erDiagram
               User {
                   UUID id
                   String first_name
                   String last_name
               }
               Org {
                   UUID id
                   String name
               }

               Org |o--o{ User : ""
           """
  end
end
