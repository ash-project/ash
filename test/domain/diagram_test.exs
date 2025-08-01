defmodule Ash.Test.Domain.Info.DiagramTest do
  @moduledoc false
  use ExUnit.Case, async: true

  test "generate a mermaid entity relationship diagram from a domain" do
    assert Ash.Domain.Info.Diagram.mermaid_er_diagram(Ash.Test.Flow.Domain) == """
           erDiagram
               "User" {
                   UUID id
                   String first_name
                   String last_name
                   String email
               }
               "Org" {
                   UUID id
                   String name
               }

               "Org" ||--|| "User" : ""
           """
  end

  test "generate a mermaid class diagram from a domain" do
    assert Ash.Domain.Info.Diagram.mermaid_class_diagram(Ash.Test.Flow.Domain) == """
           classDiagram
               class User {
                   UUID id
                   String first_name
                   String last_name
                   String email
                   Org org
                   destroy()
                   read()
                   for_org(UUID org)
                   by_name(String name)
                   create(UUID org, String first_name, String last_name, String email)
                   update(String first_name, String last_name, String email)
                   approve()
                   unapprove()
               }
               class Org {
                   UUID id
                   String name
                   User[] users
                   update(String name)
                   create(String name)
                   destroy()
                   read()
                   by_name(String name)
               }

               Org -- User
           """
  end

  test "include private fields in a mermaid entity relationship diagram from a domain if specified" do
    assert Ash.Domain.Info.Diagram.mermaid_er_diagram(Ash.Test.Flow.Domain, show_private?: true) ==
             """
             erDiagram
                 "User" {
                     UUID id
                     String first_name
                     String last_name
                     String email
                     Boolean approved
                     UUID org_id
                 }
                 "Org" {
                     UUID id
                     String name
                 }

                 "Org" ||--|| "User" : ""
             """
  end

  test "include long names in mermaid entity relationship diagram if specified" do
    assert Ash.Domain.Info.Diagram.mermaid_er_diagram([Ash.Test.Flow.Domain], name: :full) ==
             """
             erDiagram
                 "Ash.Test.Flow.User" {
                     UUID id
                     String first_name
                     String last_name
                     String email
                 }
                 "Ash.Test.Flow.Org" {
                     UUID id
                     String name
                 }

                 "Ash.Test.Flow.Org" ||--|| "Ash.Test.Flow.User" : ""
             """
  end

  test "include private fields in a mermaid class diagram from a domain if specified" do
    assert Ash.Domain.Info.Diagram.mermaid_class_diagram(Ash.Test.Flow.Domain,
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
                   for_org(UUID org)
                   by_name(String name)
                   create(UUID org, String first_name, String last_name, String email)
                   update(String first_name, String last_name, String email)
                   approve()
                   unapprove()
               }
               class Org {
                   UUID id
                   String name
                   User[] users
                   update(String name)
                   create(String name)
                   destroy()
                   read()
                   by_name(String name)
               }

               Org -- User
           """
  end
end
