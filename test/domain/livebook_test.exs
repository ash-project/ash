defmodule Ash.Test.Domain.Info.LivebookTest do
  @moduledoc false
  use ExUnit.Case, async: true

  test "generate a livebook Domain section from a given Domain" do
    assert Ash.Domain.Info.Livebook.domain_section(Ash.Test.Flow.Domain) ==
             """
             ## Domain Ash.Test.Flow.Domain

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
             | **create** | _create_ | <ul><li><b>org</b> <i>UUID</i> </li><li><b>first_name</b> <i>String</i> attribute</li><li><b>last_name</b> <i>String</i> attribute</li><li><b>email</b> <i>String</i> attribute</li></ul> |  |
             | **update** | _update_ | <ul><li><b>first_name</b> <i>String</i> attribute</li><li><b>last_name</b> <i>String</i> attribute</li><li><b>email</b> <i>String</i> attribute</li></ul> |  |
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
             | **update** | _update_ | <ul><li><b>name</b> <i>String</i> attribute</li></ul> |  |
             | **create** | _create_ | <ul><li><b>name</b> <i>String</i> attribute</li></ul> |  |
             | **destroy** | _destroy_ | <ul></ul> |  |
             | **read** | _read_ | <ul></ul> |  |
             | **by_name** | _read_ | <ul><li><b>name</b> <i>String</i> </li></ul> |  |

             """
  end
end
