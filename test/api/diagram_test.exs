defmodule Ash.Test.Api.Info.DiagramTest do
  @moduledoc false
  use ExUnit.Case, async: true

  test "generate a mermaid entity relationship diagram from an Api" do
    assert Ash.Api.Info.Diagram.mermaid_er_diagram(Ash.Test.Support.Flow.Api) == """
           erDiagram
               User {
                   UUID id
                   String first_name
                   String last_name
                   String email
               }
               Org {
                   UUID id
                   String name
               }

               Org ||--|| User : ""
           """
  end

  test "generate a mermaid class diagram from an Api" do
    assert Ash.Api.Info.Diagram.mermaid_class_diagram(Ash.Test.Support.Flow.Api) == """
           classDiagram
               class User {
                   UUID id
                   String first_name
                   String last_name
                   String email
                   Org org
                   destroy()
                   read()
                   for_org()
                   create()
                   update()
                   approve()
                   unapprove()
               }
               class Org {
                   UUID id
                   String name
                   User[] users
                   destroy()
                   update()
                   read()
                   create()
                   by_name()
               }

               Org -- User
           """
  end

  test "include private fields in a mermaid entity relationship diagram from an Api if specified" do
    assert Ash.Api.Info.Diagram.mermaid_er_diagram(Ash.Test.Support.Flow.Api, show_private?: true) ==
             """
             erDiagram
                 User {
                     UUID id
                     String first_name
                     String last_name
                     String email
                     Boolean approved
                     UUID org_id
                 }
                 Org {
                     UUID id
                     String name
                 }

                 Org ||--|| User : ""
             """
  end

  test "include private fields in a mermaid class diagram from an Api if specified" do
    assert Ash.Api.Info.Diagram.mermaid_class_diagram(Ash.Test.Support.Flow.Api,
             show_private?: true
           ) == """
           classDiagram
               class User {
                   UUID id
                   String first_name
                   String last_name
                   String email
                   Boolean approved
                   UUID org_id
                   Org org
                   destroy()
                   read()
                   for_org()
                   create()
                   update()
                   approve()
                   unapprove()
               }
               class Org {
                   UUID id
                   String name
                   User[] users
                   destroy()
                   update()
                   read()
                   create()
                   by_name()
               }

               Org -- User
           """
  end
end
