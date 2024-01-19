defmodule Ash.Test.TracerTest.AsyncLoadTest do
  @moduledoc false
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog
  require Ash.Query

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Mnesia,
      authorizers: [
        Ash.Test.Authorizer
      ]

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    relationships do
      has_many :posts, Ash.Test.TracerTest.AsyncLoadTest.Post, destination_attribute: :author_id
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Mnesia

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string
    end

    relationships do
      belongs_to :author, Author

      many_to_many :categories, Ash.Test.TracerTest.AsyncLoadTest.Category,
        through: Ash.Test.TracerTest.AsyncLoadTest.PostCategory,
        destination_attribute_on_join_resource: :category_id,
        source_attribute_on_join_resource: :post_id
    end
  end

  defmodule PostCategory do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Mnesia

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    relationships do
      belongs_to :post, Post, primary_key?: true, allow_nil?: false

      belongs_to :category, Ash.Test.TracerTest.AsyncLoadTest.Category,
        primary_key?: true,
        allow_nil?: false
    end
  end

  defmodule Category do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Mnesia

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    relationships do
      many_to_many :posts, Post,
        through: PostCategory,
        destination_attribute_on_join_resource: :post_id,
        source_attribute_on_join_resource: :category_id
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(Author)
      entry(Post)
      entry(Category)
      entry(PostCategory)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  setup do
    capture_log(fn ->
      Ash.DataLayer.Mnesia.start(Api)
    end)

    pid = self()

    events =
      [
        [:ash, :request_step],
        [:ash, :api, :read],
        [:ash, :api, :create],
        [:ash, :api, :destroy],
        [:ash, :api, :update],
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
    Api.read!(Post, tracer: Ash.Tracer.Simple)

    assert [
             %Ash.Tracer.Simple.Span{
               type: :action,
               name: "api:post.read",
               metadata: %{
                 action: :read,
                 resource: Ash.Test.TracerTest.AsyncLoadTest.Post
               }
             }
           ] = Ash.Tracer.Simple.gather_spans()
  end

  test "a read with async loads calls the tracer for each" do
    Ash.Changeset.for_create(Post, :create, %{title: "title"}, tracer: Ash.Tracer.Simple)
    |> Api.create!()

    assert [
             %Ash.Tracer.Simple.Span{
               type: :changeset,
               name: "changeset:post:create"
             },
             %Ash.Tracer.Simple.Span{
               type: :action,
               name: "api:post.create",
               metadata: %{
                 action: :create,
                 api: Ash.Test.TracerTest.AsyncLoadTest.Api,
                 resource: Ash.Test.TracerTest.AsyncLoadTest.Post
               }
             }
           ] = Ash.Tracer.Simple.gather_spans()

    assert_receive {:telemetry,
                    {[:ash, :api, :create, :start], %{system_time: _},
                     %{resource_short_name: :post}, _}}

    assert_receive {:telemetry,
                    {[:ash, :api, :create, :stop], %{duration: _}, %{resource_short_name: :post},
                     _}}

    Post
    |> Ash.Query.load(:author)
    |> Api.read!(tracer: Ash.Tracer.Simple)

    assert_receive {:telemetry,
                    {[:ash, :api, :read, :start], %{system_time: _},
                     %{resource_short_name: :post}, []}}

    assert_receive {:telemetry,
                    {[:ash, :api, :read, :start], %{system_time: _},
                     %{resource_short_name: :author}, []}}

    assert_receive {:telemetry,
                    {[:ash, :api, :read, :stop], %{duration: _, system_time: _},
                     %{resource_short_name: :author}, []}}

    assert_receive {:telemetry,
                    {[:ash, :api, :read, :stop], %{duration: _, system_time: _},
                     %{resource_short_name: :post}, []}}

    refute_receive {:telemetry, _}

    assert [
             %Ash.Tracer.Simple.Span{
               type: :action,
               name: "api:post.read",
               metadata: %{
                 action: :read,
                 resource: Ash.Test.TracerTest.AsyncLoadTest.Post
               },
               spans: spans
             }
           ] = Ash.Tracer.Simple.gather_spans()

    assert Enum.any?(spans, &(&1.name == "api:author.read"))
  end
end
