defmodule Ash.Test.Api.Info.LivebookTest do
  @moduledoc false
  use ExUnit.Case, async: true

  test "generate a livebook API section from a given API" do
    assert Ash.Api.Info.Livebook.api_section(Ash.Test.Flow.Api) ==
             """
             # API Ash.Test.Flow.Api

             ## Class Diagram

             ```mermaid
             classDiagram
                 class User {
                     UUID id
                     String first_name
                     String last_name
                     String email
                     Org org
                     destroy(UUID id, String first_name, String last_name, String email)
                     read()
                     for_org(UUID org)
                     create(UUID org, UUID id, String first_name, String last_name, ...)
                     update(UUID id, String first_name, String last_name, String email)
                     approve()
                     unapprove()
                 }
                 class Org {
                     UUID id
                     String name
                     User[] users
                     destroy(UUID id, String name)
                     update(UUID id, String name)
                     read()
                     create(UUID id, String name)
                     by_name(String name)
                 }

                 Org -- User
             ```

             ## ER Diagram

             ```mermaid
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
             ```

             ## Resources

             - [User](#user)
             - [Org](#org)

             ## User

             User model

             ### Attributes

             | Name | Type | Description |
             | ---- | ---- | ----------- |
             | **id** | UUID | PK |
             | **first_name** | String | User's first name |
             | **last_name** | String | User's last name |
             | **email** | String | User's email address |
             | **approved** | Boolean | Is the user approved? |
             | **org_id** | UUID |  |

             ## Org

             Org model

             ### Attributes

             | Name | Type | Description |
             | ---- | ---- | ----------- |
             | **id** | UUID |  |
             | **name** | String |  |


             """
  end
end
