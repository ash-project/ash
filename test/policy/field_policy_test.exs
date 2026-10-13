# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.FieldPolicyTest do
  @doc false
  use ExUnit.Case

  require Ash.Query
  require Ash.Expr

  alias Ash.Test.Support.PolicyField.{Post, Ticket, User}

  setup do
    rep =
      Ash.create!(Ash.Changeset.for_create(User, :create, %{role: :representative, points: 4}),
        authorize?: false
      )

    user =
      Ash.create!(Ash.Changeset.for_create(User, :create, %{role: :user, points: 3}),
        authorize?: false
      )

    admin =
      Ash.create!(Ash.Changeset.for_create(User, :create, %{role: :admin, points: 2}),
        authorize?: false
      )

    other_user =
      Ash.create!(Ash.Changeset.for_create(User, :create, %{role: :user, points: 1}),
        authorize?: false
      )

    [
      user: user,
      admin: admin,
      representative: rep,
      ticket:
        Ash.create!(
          Ash.Changeset.for_create(Ticket, :create, %{
            representative_id: rep.id,
            reporter_id: user.id
          })
        ),
      other_ticket:
        Ash.create!(
          Ash.Changeset.for_create(Ticket, :create, %{
            representative_id: rep.id,
            reporter_id: other_user.id
          })
        ),
      post:
        Ash.create!(
          Ash.Changeset.for_create(Post, :create, %{
            representative_id: rep.id,
            reporter_id: user.id
          })
        )
    ]
  end

  describe "just_created_by_action" do
    test "allows reading fields" do
      user =
        User
        |> Ash.Changeset.for_create(:special_create)
        |> Ash.Changeset.load(:see_if_just_created)
        |> Ash.create!()

      assert user.see_if_just_created == "was just created with special create"

      user =
        User
        |> Ash.Changeset.for_create(:create)
        |> Ash.Changeset.load(:see_if_just_created)
        |> Ash.create!()

      assert %Ash.ForbiddenField{} = user.see_if_just_created
    end
  end

  describe "introspection" do
    test "introspection returns field policies" do
      assert [
               %Ash.Policy.FieldPolicy{bypass?: true},
               %Ash.Policy.FieldPolicy{bypass?: false},
               %Ash.Policy.FieldPolicy{bypass?: false},
               %Ash.Policy.FieldPolicy{bypass?: false},
               %Ash.Policy.FieldPolicy{bypass?: false},
               %Ash.Policy.FieldPolicy{bypass?: false}
             ] =
               Ash.Policy.Info.field_policies(User)
    end
  end

  describe "loadable array of an embedded type under field policies (#2764)" do
    test "an {:array, embedded} field forbidden by field policy returns ForbiddenField, not a crash",
         %{user: user, admin: admin, post: post} do
      # `admin_notes` is `{:array, AdminNote}`: a loadable embedded type visible only to admins
      assert %Ash.ForbiddenField{field: :admin_notes, type: :attribute} =
               Post
               |> Ash.Query.filter(id == ^post.id)
               |> Ash.read_one!(authorize?: true, actor: user)
               |> Map.get(:admin_notes)

      # an admin can still read the field
      assert [] =
               Post
               |> Ash.Query.filter(id == ^post.id)
               |> Ash.read_one!(authorize?: true, actor: admin)
               |> Map.get(:admin_notes)
    end
  end

  describe "rendering fields" do
    test "when creating as a user that cannot see the field, its value is not displayed", %{
      representative: rep,
      user: user
    } do
      assert %Ash.ForbiddenField{field: :internal_status, type: :attribute} ==
               Ticket
               |> Ash.Changeset.for_create(
                 :create,
                 %{representative_id: rep.id, reporter_id: user.id, internal_status: :new},
                 authorize?: true,
                 actor: user
               )
               |> Ash.create!(authorize?: true, actor: user)
               |> Map.get(:internal_status)
    end

    test "when destroying as a user that cannot see the field, its value is not displayed", %{
      representative: rep,
      user: user
    } do
      assert %Ash.ForbiddenField{field: :internal_status, type: :attribute} ==
               Ticket
               |> Ash.Changeset.for_create(
                 :create,
                 %{representative_id: rep.id, reporter_id: user.id, internal_status: :new},
                 authorize?: true,
                 actor: user
               )
               |> Ash.create!(authorize?: false)
               |> Ash.destroy!(authorize?: true, actor: user, return_destroyed?: true)
               |> Map.get(:internal_status)
    end

    test "when reading as a user that can see the field, its value is displayed", %{
      representative: representative
    } do
      assert :representative ==
               User
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: representative)
               |> Ash.Query.filter(id == ^representative.id)
               |> Ash.read_one!(authorize?: true, actor: representative)
               |> Map.get(:role)
    end

    test "bypasses take priority over subsequent policies", %{
      representative: representative,
      admin: admin
    } do
      assert :representative ==
               User
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: admin)
               |> Ash.Query.filter(id == ^representative.id)
               |> Ash.read_one!(authorize?: true, actor: admin)
               |> Map.get(:role)
    end

    test "field policies don't interfere with data loading", %{post: post} do
      # asserting no raise as this is a regression test
      post
      |> Ash.load!(:reporter)

      Post
      |> Ash.Query.load(:reporter)
      |> Ash.read!()
    end

    test "can load a resource with a forbidden aggregate", %{
      representative: representative
    } do
      assert %Ash.ForbiddenField{field: :ticket_count, type: :aggregate} ==
               User
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: representative)
               |> Ash.Query.filter(id == ^representative.id)
               |> Ash.Query.load([:ticket_count])
               |> Ash.read_one!(authorize?: true, actor: representative)
               |> Map.get(:ticket_count)
    end

    test "when reading as a user that cant see the field, its value is not displayed", %{
      representative: representative,
      user: user
    } do
      assert %Ash.ForbiddenField{field: :role, type: :attribute} ==
               User
               |> Ash.Query.for_read(:read, %{})
               |> Ash.Query.filter(id == ^representative.id)
               |> Ash.read_one!(authorize?: true, actor: user)
               |> Map.get(:role)
    end

    test "when loading as a user that cant see the field, its value is not displayed", %{
      representative: representative,
      user: user
    } do
      assert %Ash.ForbiddenField{field: :role, type: :attribute} ==
               User
               |> Ash.Query.select([])
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: representative)
               |> Ash.Query.filter(id == ^representative.id)
               |> Ash.read_one!(authorize?: true, actor: user)
               |> Ash.load!(:role, authorize?: true)
               |> Map.get(:role)
    end

    test "when reading as a user that cant see the field with a `relates_to_actor_via` check, the value is not displayed",
         %{
           user: user,
           ticket: ticket,
           other_ticket: other_ticket
         } do
      assert nil ==
               Ticket
               |> Ash.Query.select(:status)
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Ash.Query.filter(id == ^ticket.id)
               |> Ash.read_one!()
               |> Map.get(:status)

      assert %Ash.ForbiddenField{} =
               Ticket
               |> Ash.Query.select(:status)
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Ash.Query.filter(id == ^other_ticket.id)
               |> Ash.read_one!()
               |> Map.get(:status)
    end

    test "reading is allowed through a relationship",
         %{representative: representative} do
      # someone who is allowed because it's accessed through the ticket
      assert [ticket] =
               Ticket
               |> Ash.Query.select([])
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: representative)
               |> Ash.Query.filter(reporter.ticket_count > 0)
               |> Ash.Query.load(reporter: [:ticket_count])
               |> Ash.Query.limit(1)
               |> Ash.read!(authorize?: true)

      assert is_number(ticket.reporter.ticket_count)
      assert ticket.reporter.ticket_count > 0

      # can't read the value when reading the resource directly
      assert [user] =
               User
               |> Ash.Query.select([])
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: representative)
               |> Ash.Query.load([:ticket_count])
               |> Ash.Query.limit(1)
               |> Ash.read!(authorize?: true)

      assert user.ticket_count == %Ash.ForbiddenField{
               field: :ticket_count,
               type: :aggregate
             }
    end

    test "reading is allowed through a multi level relationship",
         %{user: user} do
      assert [ticket] =
               Ticket
               |> Ash.Query.select([:internal_status])
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Ash.Query.filter(reporter_id == ^user.id)
               |> Ash.Query.load(reporter: [:tickets])
               |> Ash.read!(authorize?: true)

      assert ticket.internal_status == %Ash.ForbiddenField{
               field: :internal_status,
               type: :attribute
             }

      nested_ticket = ticket.reporter.tickets |> List.first()

      assert nested_ticket.internal_status == nil
    end

    test "conditional bypass is applied correctly",
         %{ticket: ticket, user: user, admin: admin} do
      ticket = Ash.update!(ticket, %{internal_status: :new}, authorize?: false)

      assert [user_ticket] =
               Ticket
               |> Ash.Query.select(:internal_status)
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Ash.Query.filter(id == ^ticket.id)
               |> Ash.read!()

      assert user_ticket.internal_status == %Ash.ForbiddenField{
               field: :internal_status,
               type: :attribute
             }

      assert [admin_ticket] =
               Ticket
               |> Ash.Query.select(:internal_status)
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: admin)
               |> Ash.Query.filter(id == ^ticket.id)
               |> Ash.read!()

      assert admin_ticket.internal_status == ticket.internal_status
    end
  end

  describe "private_fields" do
    test "when reading a private value and private_fields_policy is :hide, its value is not displayed",
         %{
           user: user,
           ticket: ticket
         } do
      assert %Ash.ForbiddenField{field: :top_secret, type: :attribute} ==
               Ticket
               |> Ash.Query.select(:top_secret)
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Ash.Query.filter(id == ^ticket.id)
               |> Ash.read_one!()
               |> Map.get(:top_secret)
    end

    test "when reading a private value and private_fields_policy is :show, its value is displayed",
         %{
           user: user
         } do
      assert nil ==
               User
               |> Ash.Query.select(:top_secret)
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Ash.Query.filter(id == ^user.id)
               |> Ash.read_one!()
               |> Map.get(:top_secret)
    end

    test "when reading a private value, covered by field policy the user is not supposed to see,
      it's value is not displayed",
         %{
           post: post
         } do
      assert %Ash.ForbiddenField{field: :internal_status, type: :attribute} ==
               Post
               |> Ash.Query.select(:internal_status)
               |> Ash.Query.for_read(:read, %{}, authorize?: true)
               |> Ash.Query.filter(id == ^post.id)
               |> Ash.read_one!()
               |> Map.get(:internal_status)
    end

    test "when reading a private value, covered by field policy the user is can see, its value is displayed",
         %{
           user: user,
           post: post
         } do
      assert nil ==
               Post
               |> Ash.Query.select(:internal_status)
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Ash.Query.filter(id == ^post.id)
               |> Ash.read_one!()
               |> Map.get(:internal_status)
    end
  end

  describe "filters" do
    test "filters are replaced with the appropriate field policies", %{
      representative: representative,
      user: user
    } do
      assert [] =
               User
               |> Ash.Query.select([])
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Ash.Query.filter_input(role: :representative)
               |> Ash.read!(authorize?: true)

      assert [_] =
               User
               |> Ash.Query.select([])
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: representative)
               |> Ash.Query.filter_input(role: :representative)
               |> Ash.read!(authorize?: true)
    end

    test "it's possible to filter on values that are only allowed to be accessed from a parent",
         %{
           representative: representative,
           user: user
         } do
      # someone who is allowed because it's accessed through the ticket

      assert [ticket] =
               Ticket
               |> Ash.Query.select([])
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: representative)
               |> Ash.Query.filter(reporter.ticket_count > 0)
               |> Ash.Query.load(reporter: [:ticket_count])
               |> Ash.Query.limit(1)
               |> Ash.read!()

      assert is_number(ticket.reporter.ticket_count)
      assert ticket.reporter.ticket_count > 0

      # someone who isn't allowed to see the field
      assert [ticket] =
               Ticket
               |> Ash.Query.select([])
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Ash.Query.filter(reporter.ticket_count > 0)
               |> Ash.Query.load(reporter: [:ticket_count])
               |> Ash.Query.limit(1)
               |> Ash.read!(authorize?: true)

      assert ticket.reporter.ticket_count == %Ash.ForbiddenField{
               field: :ticket_count,
               type: :aggregate
             }
    end

    test "a field policy expression on a related resource is evaluated against the related record",
         %{ticket: ticket, user: user} do
      # `User.points` is visible only when `id == ^actor(:id)`, and `user` is the ticket's reporter
      assert [_] =
               User
               |> Ash.Query.for_read(:read, %{}, actor: user)
               |> Ash.Query.filter(id == ^user.id)
               |> Ash.Query.filter_input(points: 3)
               |> Ash.read!(authorize?: true)

      assert [%{id: id}] =
               Ticket
               |> Ash.Query.for_read(:read, %{}, actor: user)
               |> Ash.Query.filter(id == ^ticket.id)
               |> Ash.Query.filter_input(reporter: [points: 3])
               |> Ash.read!(authorize?: true)

      assert id == ticket.id
    end
  end

  describe "sorts" do
    test "sorts are replaced with the appropriate field policies", %{
      user: %{id: user_id} = user
    } do
      assert [%{id: ^user_id} | _] =
               User
               |> Ash.Query.select([:points])
               |> Ash.Query.sort_input(points: :asc_nils_last)
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Ash.read!()

      assert [_, _, _, %{id: ^user_id}] =
               User
               |> Ash.Query.select([:points])
               |> Ash.Query.sort_input(points: :asc_nils_first)
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Ash.read!()

      assert [_, _, %{id: ^user_id}, _] =
               User
               |> Ash.Query.select([:points])
               |> Ash.Query.sort(points: :asc_nils_first)
               |> Ash.Query.for_read(:read, %{}, authorize?: true, actor: user)
               |> Ash.read!()
    end
  end

  describe "counting project tickets filtered by visible hours" do
    alias Ash.Test.Support.PolicyField.{HoursBenchmark, Project, ProjectTicket}

    setup do
      for id <- [1, 2] do
        Project
        |> Ash.Changeset.for_create(:create, %{id: id})
        |> Ash.create!()
      end

      for {id, hours, owner_id, project_id} <- [
            {1, 5, 1, 1},
            {2, 20, 1, 1},
            {3, 30, 2, 1},
            {4, 40, 2, 1},
            {5, 50, 2, 1},
            {6, 60, 1, 2}
          ] do
        ProjectTicket
        |> Ash.Changeset.for_create(:create, %{
          id: id,
          hours: hours,
          owner_id: owner_id,
          project_id: project_id
        })
        |> Ash.create!(authorize?: false)
      end

      %{actor: %{id: 1}}
    end

    test "count project tickets using only hours the actor can filter", context do
      query =
        ProjectTicket
        |> Ash.Query.filter_input(%{hours: %{greater_than: 10}})
        |> Ash.Query.page(limit: 10, count: true)
        |> Ash.Query.for_read(:read, %{}, actor: context.actor)

      direct =
        query
        |> Ash.Query.filter_input(%{project_id: 1})
        |> Ash.Query.for_read(:read, %{}, actor: context.actor)
        |> Ash.read!(page: [limit: 10, count: true])

      assert Enum.map(direct.results, & &1.id) == [2]
      assert direct.count == 1

      project =
        Project
        |> Ash.Query.filter(id == 1)
        |> Ash.Query.load(tickets: query)
        |> Ash.Query.for_read(:read, %{}, actor: context.actor)
        |> Ash.read_one!()

      assert Enum.map(project.tickets.results, & &1.id) == [2]
      assert project.tickets.count == direct.count
    end

    test "count project tickets matched by public benchmarks using only visible hours" do
      for {id, hours} <- [{1, 30}, {2, 60}] do
        HoursBenchmark
        |> Ash.Changeset.for_create(:create, %{id: id, hours: hours})
        |> Ash.create!()
      end

      for {mode, actor, expected_ids} <- [
            {:input, %{id: 1}, []},
            {:input, %{id: 2}, [3]},
            {:trusted, %{id: 1}, [3]}
          ] do
        query =
          case mode do
            :input ->
              Ash.Query.filter_input(
                ProjectTicket,
                Ash.Expr.expr(exists(HoursBenchmark, hours == parent(hours)))
              )

            :trusted ->
              Ash.Query.filter(ProjectTicket, exists(HoursBenchmark, hours == parent(hours)))
          end
          |> Ash.Query.page(limit: 10, count: true)
          |> Ash.Query.for_read(:read, %{}, actor: actor)

        direct =
          query
          |> Ash.Query.filter(project_id == 1)
          |> Ash.Query.for_read(:read, %{}, actor: actor)
          |> Ash.read!()

        assert Enum.map(direct.results, & &1.id) == expected_ids
        assert direct.count == length(expected_ids)

        project =
          Project
          |> Ash.Query.filter(id == 1)
          |> Ash.Query.load(tickets: query)
          |> Ash.Query.for_read(:read, %{}, actor: actor)
          |> Ash.read_one!()

        assert Enum.map(project.tickets.results, & &1.id) == expected_ids
        assert project.tickets.count == direct.count
      end
    end

    test "count zero project tickets when only hidden hours match the filter", context do
      query =
        ProjectTicket
        |> Ash.Query.filter_input(%{hours: %{greater_than: 35}})
        |> Ash.Query.page(limit: 10, count: true)
        |> Ash.Query.for_read(:read, %{}, actor: context.actor)

      project =
        Project
        |> Ash.Query.filter(id == 1)
        |> Ash.Query.load(tickets: query)
        |> Ash.Query.for_read(:read, %{}, actor: context.actor)
        |> Ash.read_one!()

      assert project.tickets.results == []
      assert project.tickets.count == 0
    end

    test "count project tickets in an aggregate using only hours the actor can filter",
         context do
      project =
        Project
        |> Ash.Query.filter(id == 1)
        |> Ash.Query.aggregate(:long_ticket_count, :count, :tickets,
          query: Ash.Query.filter_input(ProjectTicket, %{hours: %{greater_than: 10}}),
          default: 0
        )
        |> Ash.Query.for_read(:read, %{}, actor: context.actor)
        |> Ash.read_one!()

      assert project.aggregates.long_ticket_count == 1
    end
  end
end
