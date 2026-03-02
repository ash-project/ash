# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.TracerTest.AsyncLoadTest do
  @moduledoc false
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog
  alias Ash.Test.Domain, as: Domain

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Mnesia,
      authorizers: [
        Ash.Test.Authorizer
      ]

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    calculations do
      calculate :name_length, :integer, fn records, _ ->
        Enum.map(records, fn record ->
          String.length(record.name)
        end)
      end
    end

    relationships do
      has_many :posts, Ash.Test.TracerTest.AsyncLoadTest.Post,
        destination_attribute: :author_id,
        public?: true
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Mnesia

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
      end
    end

    relationships do
      belongs_to :author, Author, public?: true

      many_to_many :categories, Ash.Test.TracerTest.AsyncLoadTest.Category,
        through: Ash.Test.TracerTest.AsyncLoadTest.PostCategory,
        destination_attribute_on_join_resource: :category_id,
        source_attribute_on_join_resource: :post_id,
        public?: true
    end
  end

  defmodule PostCategory do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    relationships do
      belongs_to :post, Post, primary_key?: true, allow_nil?: false, public?: true

      belongs_to :category, Ash.Test.TracerTest.AsyncLoadTest.Category,
        primary_key?: true,
        allow_nil?: false,
        public?: true
    end
  end

  defmodule Category do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    relationships do
      many_to_many :posts, Post,
        through: PostCategory,
        destination_attribute_on_join_resource: :post_id,
        source_attribute_on_join_resource: :category_id,
        public?: true
    end
  end

  setup do
    capture_log(fn ->
      Ash.DataLayer.Mnesia.start(Domain, [Author, Post, Category, PostCategory])
    end)

    pid = self()

    events =
      [
        [:ash, :request_step],
        [:ash, :domain, :read],
        [:ash, :domain, :create],
        [:ash, :domain, :destroy],
        [:ash, :domain, :update],
        [:ash, :calculation],
        [:ash, :flow]
      ]
      |> Enum.flat_map(fn list ->
        [list ++ [:start], list ++ [:stop]]
      end)

    capture_log(fn ->
      :telemetry.attach_many(
        :test_handler,
        events,
        fn event_name, event_measurements, event_metadata, handler_config ->
          send(
            pid,
            {:telemetry, {event_name, event_measurements, event_metadata, handler_config}}
          )
        end,
        []
      )
    end)

    on_exit(fn ->
      :telemetry.detach(:test_handler)

      capture_log(fn ->
        :mnesia.stop()
        :mnesia.delete_schema([node()])
      end)
    end)

    start_supervised(
      {Ash.Test.Authorizer,
       strict_check: :authorized,
       check: {:error, Ash.Error.Forbidden.exception([])},
       strict_check_context: [:query]}
    )

    :ok
  end

  test "a simple read calls the tracer with the action" do
    Ash.read!(Post, tracer: Ash.Tracer.Simple)

    assert [
             %Ash.Tracer.Simple.Span{
               type: :action,
               name: "domain:post.read",
               metadata: %{
                 action: :read,
                 resource: Ash.Test.TracerTest.AsyncLoadTest.Post
               }
             }
           ] = Ash.Tracer.Simple.gather_spans()
  end

  test "a read with async loads calls the tracer for each" do
    Ash.Changeset.for_create(Post, :create, %{title: "title"}, tracer: Ash.Tracer.Simple)
    |> Ash.create!()

    assert [
             %Ash.Tracer.Simple.Span{
               type: :changeset,
               name: "changeset:post:create"
             },
             %Ash.Tracer.Simple.Span{
               type: :action,
               name: "domain:post.create",
               metadata: %{
                 action: :create,
                 domain: Ash.Test.Domain,
                 resource: Ash.Test.TracerTest.AsyncLoadTest.Post
               }
             }
           ] = Ash.Tracer.Simple.gather_spans()

    assert_receive {:telemetry,
                    {[:ash, :domain, :create, :start], %{system_time: _},
                     %{resource_short_name: :post}, _}}

    assert_receive {:telemetry,
                    {[:ash, :domain, :create, :stop], %{duration: _},
                     %{resource_short_name: :post}, _}}

    Post
    |> Ash.Query.load(:author)
    |> Ash.read!(tracer: Ash.Tracer.Simple)

    assert_receive {:telemetry,
                    {[:ash, :domain, :read, :start], %{system_time: _},
                     %{resource_short_name: :post}, []}}

    assert_receive {:telemetry,
                    {[:ash, :domain, :read, :start], %{system_time: _},
                     %{resource_short_name: :author}, []}}

    assert_receive {:telemetry,
                    {[:ash, :domain, :read, :stop], %{duration: _, system_time: _},
                     %{resource_short_name: :author}, []}}

    assert_receive {:telemetry,
                    {[:ash, :domain, :read, :stop], %{duration: _, system_time: _},
                     %{resource_short_name: :post}, []}}

    refute_receive {:telemetry, _}

    assert [
             %Ash.Tracer.Simple.Span{
               type: :action,
               name: "domain:post.read",
               metadata: %{
                 action: :read,
                 resource: Ash.Test.TracerTest.AsyncLoadTest.Post
               },
               spans: spans
             }
           ] = Ash.Tracer.Simple.gather_spans()

    assert Enum.any?(spans, &(&1.name == "domain:author.read"))
  end

  test "a read with calculation loads calls the tracer" do
    Ash.Changeset.for_create(Author, :create, %{name: "anc"})
    |> Ash.create!()

    assert_receive {:telemetry,
                    {[:ash, :domain, :create, :start], %{system_time: _},
                     %{resource_short_name: :author}, _}}

    assert_receive {:telemetry,
                    {[:ash, :domain, :create, :stop], %{duration: _},
                     %{resource_short_name: :author}, _}}

    Author
    |> Ash.Query.load(:name_length)
    |> Ash.read!(tracer: Ash.Tracer.Simple)

    assert [
             %Ash.Tracer.Simple.Span{
               type: :action,
               name: "domain:author.read",
               metadata: %{
                 action: :read,
                 resource: Ash.Test.TracerTest.AsyncLoadTest.Author
               },
               spans: [
                 %Ash.Tracer.Simple.Span{
                   name: "author:calculation:name_length",
                   type: :calculation,
                   metadata: %{
                     resource: Ash.Test.TracerTest.AsyncLoadTest.Author,
                     resource_short_name: :author,
                     calculation: "name_length",
                     authorize?: true
                   },
                   spans: []
                 }
                 | _
               ]
             }
           ] = Ash.Tracer.Simple.gather_spans()

    assert_receive {:telemetry,
                    {[:ash, :domain, :read, :start], %{system_time: _},
                     %{resource_short_name: :author}, []}}

    assert_receive {:telemetry,
                    {[:ash, :domain, :read, :stop], %{duration: _, system_time: _},
                     %{resource_short_name: :author}, []}}

    assert_receive {:telemetry,
                    {[:ash, :calculation, :start], %{system_time: _},
                     %{resource_short_name: :author, calculation: "name_length"}, []}}

    assert_receive {:telemetry,
                    {[:ash, :calculation, :stop], %{duration: _, system_time: _},
                     %{resource_short_name: :author, calculation: "name_length"}, []}}

    refute_receive {:telemetry, _}
  end
end

defmodule Ash.Test.TracerTest.BulkActionsTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      update :atomic_upgrade_update do
        atomic_upgrade? true
      end

      update :non_atomic_update do
        require_atomic? false
        atomic_upgrade? false
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public? true
      end
    end
  end

  setup do
    pid = self()

    events =
      [
        [:ash, :domain, :bulk_create],
        [:ash, :domain, :bulk_update],
        [:ash, :domain, :bulk_destroy]
      ]
      |> Enum.flat_map(fn list ->
        [list ++ [:start], list ++ [:stop]]
      end)

    :telemetry.attach_many(
      :bulk_tracer_test_handler,
      events,
      fn event_name, event_measurements, event_metadata, handler_config ->
        send(
          pid,
          {:telemetry, {event_name, event_measurements, event_metadata, handler_config}}
        )
      end,
      []
    )

    on_exit(fn ->
      :telemetry.detach(:bulk_tracer_test_handler)
    end)

    :ok
  end

  test "bulk_create produces a :bulk_create span" do
    Ash.bulk_create!([%{title: "post1"}, %{title: "post2"}], Post, :create,
      tracer: Ash.Tracer.Simple
    )

    spans = Ash.Tracer.Simple.gather_spans()

    assert [
             %Ash.Tracer.Simple.Span{
               type: :bulk_create,
               name: "domain:post.create",
               metadata: %{
                 action: :create,
                 resource: Ash.Test.TracerTest.BulkActionsTest.Post
               }
             }
           ] = top_level_spans(spans, :bulk_create)
  end

  test "bulk_create produces telemetry events" do
    Ash.bulk_create!([%{title: "post1"}], Post, :create, tracer: Ash.Tracer.Simple)

    assert_receive {:telemetry,
                    {[:ash, :domain, :bulk_create, :start], %{system_time: _},
                     %{resource_short_name: :post}, _}}

    assert_receive {:telemetry,
                    {[:ash, :domain, :bulk_create, :stop], %{duration: _},
                     %{resource_short_name: :post}, _}}
  end

  test "bulk_create produces :bulk_batch spans" do
    Ash.bulk_create!([%{title: "post1"}, %{title: "post2"}], Post, :create,
      tracer: Ash.Tracer.Simple,
      batch_size: 2
    )

    spans = Ash.Tracer.Simple.gather_spans()

    bulk_create_span = find_span(spans, :bulk_create)
    assert bulk_create_span

    batch_spans = find_nested_spans(bulk_create_span, :bulk_batch)
    assert length(batch_spans) >= 1
  end

  test "bulk_update produces a :bulk_update span" do
    Ash.bulk_create!([%{title: "post1"}, %{title: "post2"}], Post, :create)

    Ash.bulk_update!(Post, :update, %{title: "updated"},
      tracer: Ash.Tracer.Simple,
      strategy: :stream,
      return_errors?: true
    )

    spans = Ash.Tracer.Simple.gather_spans()

    assert [
             %Ash.Tracer.Simple.Span{
               type: :bulk_update,
               name: "domain:post.update",
               metadata: %{
                 action: :update,
                 resource: Ash.Test.TracerTest.BulkActionsTest.Post
               }
             }
           ] = top_level_spans(spans, :bulk_update)
  end

  test "bulk_update produces telemetry events" do
    Ash.bulk_create!([%{title: "post1"}], Post, :create)

    Ash.bulk_update!(Post, :update, %{title: "updated"},
      tracer: Ash.Tracer.Simple,
      strategy: :stream,
      return_errors?: true
    )

    assert_receive {:telemetry,
                    {[:ash, :domain, :bulk_update, :start], %{system_time: _},
                     %{resource_short_name: :post}, _}}

    assert_receive {:telemetry,
                    {[:ash, :domain, :bulk_update, :stop], %{duration: _},
                     %{resource_short_name: :post}, _}}
  end

  test "bulk_update with stream strategy produces :bulk_batch spans" do
    Ash.bulk_create!([%{title: "post1"}, %{title: "post2"}], Post, :create)

    Ash.bulk_update!(Post, :update, %{title: "updated"},
      tracer: Ash.Tracer.Simple,
      strategy: :stream,
      batch_size: 2,
      return_errors?: true
    )

    spans = Ash.Tracer.Simple.gather_spans()

    bulk_update_span = find_span(spans, :bulk_update)
    assert bulk_update_span

    batch_spans = find_nested_spans(bulk_update_span, :bulk_batch)
    assert length(batch_spans) >= 1
  end

  test "bulk_destroy produces a :bulk_destroy span" do
    Ash.bulk_create!([%{title: "post1"}, %{title: "post2"}], Post, :create)

    Ash.bulk_destroy!(Post, :destroy, %{},
      tracer: Ash.Tracer.Simple,
      strategy: :stream,
      return_errors?: true
    )

    spans = Ash.Tracer.Simple.gather_spans()

    assert [
             %Ash.Tracer.Simple.Span{
               type: :bulk_destroy,
               name: "domain:post.destroy",
               metadata: %{
                 action: :destroy,
                 resource: Ash.Test.TracerTest.BulkActionsTest.Post
               }
             }
           ] = top_level_spans(spans, :bulk_destroy)
  end

  test "bulk_destroy produces telemetry events" do
    Ash.bulk_create!([%{title: "post1"}], Post, :create)

    Ash.bulk_destroy!(Post, :destroy, %{},
      tracer: Ash.Tracer.Simple,
      strategy: :stream,
      return_errors?: true
    )

    assert_receive {:telemetry,
                    {[:ash, :domain, :bulk_destroy, :start], %{system_time: _},
                     %{resource_short_name: :post}, _}}

    assert_receive {:telemetry,
                    {[:ash, :domain, :bulk_destroy, :stop], %{duration: _},
                     %{resource_short_name: :post}, _}}
  end

  test "bulk_destroy with stream strategy produces :bulk_batch spans" do
    Ash.bulk_create!([%{title: "post1"}, %{title: "post2"}], Post, :create)

    Ash.bulk_destroy!(Post, :destroy, %{},
      tracer: Ash.Tracer.Simple,
      strategy: :stream,
      batch_size: 2,
      return_errors?: true
    )

    spans = Ash.Tracer.Simple.gather_spans()

    bulk_destroy_span = find_span(spans, :bulk_destroy)
    assert bulk_destroy_span

    batch_spans = find_nested_spans(bulk_destroy_span, :bulk_batch)
    assert length(batch_spans) >= 1
  end

  test "atomic upgraded update produces an :action span, not a :bulk_update span" do
    post =
      Ash.Changeset.for_create(Post, :create, %{title: "original"})
      |> Ash.create!()

    # The :atomic_upgrade_update action has atomic_upgrade? true.
    # ETS supports update_query and expr_error, so this will be atomically
    # upgraded to go through Update.Bulk.run, but it should still emit
    # an :action span since it was a single-record update call.
    Ash.Changeset.for_update(post, :atomic_upgrade_update, %{title: "updated"},
      tracer: Ash.Tracer.Simple
    )
    |> Ash.update!(tracer: Ash.Tracer.Simple, authorize?: false)

    spans = Ash.Tracer.Simple.gather_spans()

    # Even though the atomic upgrade goes through Update.Bulk.run internally,
    # it should produce an :action span (not :bulk_update) since it was a
    # single-record Ash.update! call.
    assert find_span(spans, :action)
    refute find_span(spans, :bulk_update)
  end

  test "non-atomic update produces :action span and no :bulk_update span" do
    post =
      Ash.Changeset.for_create(Post, :create, %{title: "original"})
      |> Ash.create!()

    Ash.Changeset.for_update(post, :non_atomic_update, %{title: "updated"},
      tracer: Ash.Tracer.Simple
    )
    |> Ash.update!(tracer: Ash.Tracer.Simple)

    spans = Ash.Tracer.Simple.gather_spans()

    action_spans = top_level_spans(spans, :action)
    bulk_update_spans = top_level_spans(spans, :bulk_update)

    assert action_spans != []
    assert bulk_update_spans == []
  end

  defp top_level_spans(spans, type) do
    Enum.filter(spans, &(&1.type == type))
  end

  defp find_span(spans, type) do
    Enum.find(spans, &(&1.type == type)) ||
      Enum.find_value(spans, fn span ->
        find_span(span.spans, type)
      end)
  end

  defp find_nested_spans(%{spans: spans}, type) do
    direct = Enum.filter(spans, &(&1.type == type))

    nested =
      Enum.flat_map(spans, fn span ->
        find_nested_spans(span, type)
      end)

    direct ++ nested
  end
end
