# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.AuthorizationTracerTest do
  @moduledoc false
  use ExUnit.Case, async: false

  require Ash.Query

  defmodule RuntimeCheck do
    @moduledoc false
    use Ash.Policy.Check

    def describe(_), do: "record is allowed"
    def strict_check(_, _, _), do: {:ok, :unknown}
    def check(_, records, _, _), do: Enum.filter(records, & &1.allowed)
  end

  defmodule RaisingCheck do
    @moduledoc false
    use Ash.Policy.SimpleCheck

    def describe(_), do: "raises"
    def match?(_, _, _), do: raise("authorization failed")
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.AuthorizationTracerTest.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    actions do
      defaults [:read, create: :*]
    end

    policies do
      policy always() do
        authorize_if always()
      end
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.AuthorizationTracerTest.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :allowed, :boolean, default: true, public?: true
    end

    relationships do
      belongs_to :author, Author, public?: true
    end

    actions do
      defaults [:read, :destroy, create: :*, update: :*]
      read :runtime
      read :raising
      read :denied
    end

    policies do
      policy action(:runtime) do
        access_type :runtime
        authorize_if RuntimeCheck
      end

      policy action(:raising) do
        authorize_if RaisingCheck
      end

      policy action(:denied) do
        authorize_if never()
      end

      policy action([:read, :create, :update, :destroy]) do
        authorize_if always()
      end
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain

    resources do
      resource Author
      resource Post
    end
  end

  defmodule FilteredTracer do
    @moduledoc false
    use Ash.Tracer

    def trace_type?(type), do: type != :authorization
    defdelegate start_span(type, name), to: Ash.Tracer.Simple
    defdelegate stop_span(), to: Ash.Tracer.Simple
    defdelegate set_metadata(type, metadata), to: Ash.Tracer.Simple
    defdelegate set_error(error), to: Ash.Tracer.Simple
    defdelegate get_span_context(), to: Ash.Tracer.Simple
    defdelegate set_span_context(context), to: Ash.Tracer.Simple
  end

  setup do
    handler = {__MODULE__, make_ref()}

    :telemetry.attach_many(
      handler,
      [[:ash, :authorization, :start], [:ash, :authorization, :stop]],
      &__MODULE__.handle_event/4,
      self()
    )

    on_exit(fn -> :telemetry.detach(handler) end)
    :ok
  end

  def handle_event(event, measurements, metadata, pid) do
    send(pid, {:authorization, event, measurements, metadata})
  end

  test "authorization spans are children of the action and identify the phase" do
    assert [] = Ash.read!(Post, authorize?: true, tracer: Ash.Tracer.Simple)
    assert [%{type: :action} = action] = Ash.Tracer.Simple.gather_spans()

    for phase <- [:main, :related_queries] do
      assert span = Enum.find(action.spans, &(&1.metadata[:phase] == phase))
      assert span.type == :authorization
      assert span.parent_id == action.id
      assert span.duration >= 0

      assert span.metadata == %{
               domain: Domain,
               resource: Post,
               resource_short_name: :post,
               action: :read,
               phase: phase
             }

      assert_receive {:authorization, [:ash, :authorization, :start], %{system_time: _},
                      %{phase: ^phase, resource: Post, action: :read}}

      assert_receive {:authorization, [:ash, :authorization, :stop], %{duration: duration},
                      %{phase: ^phase, resource: Post, action: :read}}

      assert duration >= 0
    end
  end

  test "related paths are grouped without tracing each policy solve" do
    author = Ash.create!(Author, %{name: "visible"}, authorize?: false)
    post = Ash.create!(Post, %{author_id: author.id}, authorize?: false)
    Ash.Tracer.Simple.gather_spans()

    assert [result] =
             Post
             |> Ash.Query.filter(author.name == "visible")
             |> Ash.read!(authorize?: true, tracer: Ash.Tracer.Simple)

    assert result.id == post.id
    assert [%{type: :action} = action] = Ash.Tracer.Simple.gather_spans()
    assert [related] = Enum.filter(action.spans, &(&1.metadata[:phase] == :related_queries))

    refute Enum.any?(authorization_spans(related.spans), fn span ->
             span.metadata.phase == :related_queries
           end)
  end

  test "deferred runtime checks are traced and preserve record filtering" do
    allowed = Ash.create!(Post, %{allowed: true}, authorize?: false)
    Ash.create!(Post, %{allowed: false}, authorize?: false)

    assert [result] =
             Post
             |> Ash.Query.for_read(:runtime)
             |> Ash.read!(authorize?: true, tracer: Ash.Tracer.Simple)

    assert result.id == allowed.id
    spans = authorization_spans(Ash.Tracer.Simple.gather_spans())
    assert Enum.any?(spans, &(&1.metadata.phase == :runtime))
  end

  test "the global tracer is respected and deduplicated" do
    previous = Application.get_env(:ash, :tracer)
    Application.put_env(:ash, :tracer, Ash.Tracer.Simple)

    on_exit(fn ->
      if previous do
        Application.put_env(:ash, :tracer, previous)
      else
        Application.delete_env(:ash, :tracer)
      end
    end)

    assert Ash.can?({Post, :read}, nil, tracer: Ash.Tracer.Simple)
    assert [%{type: :authorization}] = authorization_spans(Ash.Tracer.Simple.gather_spans())
  end

  test "standalone permission checks inherit a tracer from the subject" do
    query = Ash.Query.for_read(Post, :read, %{}, tracer: Ash.Tracer.Simple)
    Ash.Tracer.Simple.gather_spans()
    assert Ash.can?(query, nil)

    assert [%{type: :authorization, metadata: %{phase: :main}}] =
             Ash.Tracer.Simple.gather_spans()
  end

  test "telemetry works without a tracer and forbidden results are unchanged" do
    post = Ash.create!(Post, %{}, authorize?: false)
    refute Ash.can?({Post, :denied}, nil, data: post)
    assert_receive {:authorization, [:ash, :authorization, :start], _, %{phase: :main}}

    assert_receive {:authorization, [:ash, :authorization, :stop], %{duration: _},
                    %{phase: :main}}
  end

  test "tracer type filtering does not disable telemetry" do
    assert Ash.can?({Post, :read}, nil, tracer: FilteredTracer)
    assert [] = Ash.Tracer.Simple.gather_spans()
    assert_receive {:authorization, [:ash, :authorization, :stop], _, %{phase: :main}}
  end

  test "raised checks close both spans and allow subsequent tracing" do
    assert_raise RuntimeError, "authorization failed", fn ->
      Ash.can?({Post, :raising}, nil, tracer: Ash.Tracer.Simple)
    end

    assert [%{type: :authorization, error: %RuntimeError{}}] =
             Ash.Tracer.Simple.gather_spans()

    assert_receive {:authorization, [:ash, :authorization, :stop], _, %{phase: :main}}
    assert Ash.can?({Post, :read}, nil, tracer: Ash.Tracer.Simple)
    assert [%{type: :authorization, parent_id: nil}] = Ash.Tracer.Simple.gather_spans()
  end

  test "actions with authorization disabled do not emit authorization spans" do
    Ash.read!(Post, authorize?: false, tracer: Ash.Tracer.Simple)
    assert [] = authorization_spans(Ash.Tracer.Simple.gather_spans())
    refute_receive {:authorization, _, _, _}
  end

  defp authorization_spans(spans) do
    Enum.flat_map(spans, fn span ->
      if span.type == :authorization do
        [span | authorization_spans(span.spans)]
      else
        authorization_spans(span.spans)
      end
    end)
  end
end
