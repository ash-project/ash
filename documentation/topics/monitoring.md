# Instrumentation

Instrumentation Ash has two primary components, {{link:ash:module:Ash.Tracer}} and `:telemetry`. Instrumentation is closely tied to observability and monitoring.

## Telemetry

Ash emits the following telemetry events, suffixed with `:start` and `:stop`. Start events have `system_time` measurements, and stop events have `system_time` and `duration` measurements. All times will be in the native time unit.

- `[:ash, <api_short_name>, :create]` - The execution of a create action. Use `resource_short_name` and `action` metadata to break down measurements.
- `[:ash, <api_short_name>, :update]` - The execution of a update action. Use `resource_short_name` and `action` metadata to break down measurements.
- `[:ash, <api_short_name>, :read]` - The execution of a read action. Use `resource_short_name` and `action` metadata to break down measurements.
- `[:ash, <api_short_name>, :destroy]` - The execution of a destroy action. Use `resource_short_name` and `action` metadata to break down measurements.
- `[:ash, :changeset]` - A changeset being processed for a given action, i.e with `Ash.Changeset.for_create`. Use `resource_short_name` metadata to break down measurements.
- `[:ash, :query]` - A query being processed for an action, with `Ash.Query.for_read`. Use `resource_short_name` metadata to break down measurements.
- `[:ash, :validation]` - A validation being run on a changeset. Use `resource_short_name` and `validation` metadata to break down measurements.
- `[:ash, :change]` - A change being run on a changeset. Use `resource_short_name` and `change` metadata to break down measurements.
- `[:ash, :before_action]` - A before_action being run on a changeset. Use `resource_short_name` to break down measurements. 
- `[:ash, :after_action]` - An after_action being run on a changeset. Use `resource_short_name` to break down measurements.
- `[:ash, :preparation]` - A preparation being run on a changeset. Use `resource_short_name` and `preparation` metadata to break down measurements.
- `[:ash, :request_step]` - The resolution of an internal request. Ash breaks up its operations internally into multiple requests, this can give you a high resolution insight onto the execution of those internal requests resolution. Use `name` metadata to break down measurements.
- `[:ash, :flow]` - The execution of an Ash flow. Use `flow_short_name` to break down measurements.
- `[:ash, :flow, :custom_step]` - The execution of a custom flow step (only if using the built in runner, which is currently the only runner). Use `flow_short_name` and `name` metadata to break down measurements. 

## Tracing

Tracing is very similar to telemetry, but gives you some additional hooks to `set_span_context()` and `get_span_context()`. This allows you to "move" some piece of context between two processes. Ash will call this whenever it starts a new process to do anything. What this means is that if you are using a tracing tool or library you can ensure that any processes spawned by Ash are properly included in the trace. Additionally, you should be able to integrate a tracing library to include Ash actions/spans relatively easily by implementing the other callbacks.

A tracer can be configured globally in application config.

```elixir
config :ash, :tracer, MyApp.Tracer
```

Additionally, one can be provide when creating changesets or calling an api, i.e

```elixir
Resource
# better to put it here, as changesets are traced as well. It will be carried over to the api action
|> Ash.Changeset.for_create(:create, %{}, tracer: MyApp.Tracer)**** 
# but you can also pass it here.
|> Api.create!(tracer: MyApp.Tracer)
```

For customizing the names created for each span, see:

- {{link:ash:option:api/execution/trace_name}}
- {{link:ash:option:resource/resource/trace_name}}
- {{link:ash:option:flow/flow/trace_name}}


## After/Before Action Hooks

Due to the way before/after action hooks run, their execution time won't be included in the span created for the change. In practice, before/after action hooks are where the long running operations tend to be. We start a corresponding `span` and emit a telemetry event for before and after hooks, but they are only so useful. In a trace, they can highlight that "some hook" took a long time. In telemetry metrics they are of even less use. The cardinality of the metric would be extremely high, and we don't have a "name" or anything to distinguish them. To that end, you can use the macros & functions available in `Ash.Tracer` to create custom spans and/or emit custom telemetry events from your hooks. They automatically handle cases where the provided tracer is `nil`, for convenience. For example:

```elixir
defmodule MyApp.CustomChange do
  use Ash.Resource.Change

  require Ash.Tracer

  def change(changeset, _, _) do
    changeset
    |> Ash.Changeset.before_action(fn changeset -> 
      Ash.Tracer.span(:custom, "custom name", changeset.context[:private][:tracer]) do
        # optionally set some metadata
        metadata = %{...}
        Ash.Tracer.set_metadata(changeset.context[:private][:tracer], :custom, metadata)
        # will get `:start` and `:stop` suffixed events emitted
        Ash.Tracer.telemetry_span([:telemetry, :event, :name], metadata) do
          ## Your logic here
        end
      end
    end)
  end
end
```
