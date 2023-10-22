defmodule Ash.Test.Api.Info.LivebookTest do
  @moduledoc false
  use ExUnit.Case, async: true

  test "generate a livebook API section from a given API" do
    assert Ash.Api.Info.Livebook.api_section(Ash.Test.Flow.Api) ==
             """
             ## API Ash.Test.Flow.Api

             ### Class Diagram

             ```mermaid
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
                     create(UUID org, UUID id, String first_name, String last_name, ...)
                     update(UUID id, String first_name, String last_name, String email)
                     approve()
                     unapprove()
                 }
                 class Org {
                     UUID id
                     String name
                     User[] users
                     destroy()
                     update(UUID id, String name)
                     read()
                     create(UUID id, String name)
                     by_name(String name)
                 }

                 Org -- User
             ```

             ### ER Diagram

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

             ### Resources

             - [User](#user)
             - [Org](#org)

             ### User

             User model

             #### Attributes

             | Name | Type | Description |
             | ---- | ---- | ----------- |
             | **id** | UUID | PK |
             | **first_name** | String | User's first name |
             | **last_name** | String | User's last name |
             | **email** | String | User's email address |
             | **approved** | Boolean | Is the user approved? |
             | **org_id** | UUID |  |

             #### Actions

             | Name | Type | Input | Description |
             | ---- | ---- | ----- | ----------- |
             | **destroy** | _destroy_ | <ul></ul> |  |
             | **read** | _read_ | <ul></ul> |  |
             | **for_org** | _read_ | <ul><li><b>org</b> <i>UUID</i> </li></ul> |  |
             | **by_name** | _read_ | <ul><li><b>name</b> <i>String</i> </li></ul> |  |
             | **create** | _create_ | <ul><li><b>org</b> <i>UUID</i> </li><li><b>id</b> <i>UUID</i> attribute</li><li><b>first_name</b> <i>String</i> attribute</li><li><b>last_name</b> <i>String</i> attribute</li><li><b>email</b> <i>String</i> attribute</li></ul> |  |
             | **update** | _update_ | <ul><li><b>id</b> <i>UUID</i> attribute</li><li><b>first_name</b> <i>String</i> attribute</li><li><b>last_name</b> <i>String</i> attribute</li><li><b>email</b> <i>String</i> attribute</li></ul> |  |
             | **approve** | _update_ | <ul></ul> |  |
             | **unapprove** | _update_ | <ul></ul> |  |

             ### Org

             Org model

             #### Attributes

             | Name | Type | Description |
             | ---- | ---- | ----------- |
             | **id** | UUID |  |
             | **name** | String |  |

             #### Actions

             | Name | Type | Input | Description |
             | ---- | ---- | ----- | ----------- |
             | **destroy** | _destroy_ | <ul></ul> |  |
             | **update** | _update_ | <ul><li><b>id</b> <i>UUID</i> attribute</li><li><b>name</b> <i>String</i> attribute</li></ul> |  |
             | **read** | _read_ | <ul></ul> |  |
             | **create** | _create_ | <ul><li><b>id</b> <i>UUID</i> attribute</li><li><b>name</b> <i>String</i> attribute</li></ul> |  |
             | **by_name** | _read_ | <ul><li><b>name</b> <i>String</i> </li></ul> |  |

             """
  end
end
