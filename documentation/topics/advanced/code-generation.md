<!--
SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>

SPDX-License-Identifier: MIT
-->

# Code Generation with the Manifest

Code generation extensions like `AshGraphql`, `AshJsonApi`, `AshTypescript`, and
OpenAPI generators have historically each had to walk Ash's internal DSL state
to discover what to emit. That works — but every extension re-derives the same
shape: which resources exist, what fields they have, what types those fields
have, which operators apply, which actions are reachable, and so on. The
introspection logic ends up duplicated across extensions, drift accumulates,
and the dependency on private Ash internals makes each extension fragile to
framework refactors.

`Ash.Info.Manifest` is a single normalized data structure that captures all of
that shape in one pass. It's the recommended starting point for any new
code-generation extension. This guide describes the pattern.

## The manifest

`Ash.Info.Manifest.generate/1` walks an OTP app's domains and produces a struct
that contains:

  * `resources` — every reachable resource (modules whose types you reach
    transitively from public actions), with their public fields, relationships,
    identities, and multitenancy config.
  * `types` — full definitions of every named type referenced (enums,
    NewTypes, embedded resources).
  * `entrypoints` — one per public action on each reachable resource.
  * `filter_capabilities` — the universe of operators, functions, and custom
    expressions available, with per-field applicability already resolved.
  * `sort_capabilities` — sort directions.

```elixir
{:ok, manifest} = Ash.Info.Manifest.generate(otp_app: :my_app)
```

The struct is plain data — no Ash runtime APIs — and can be serialized to JSON
via `Ash.Info.Manifest.JsonSerializer.to_map/1` for use by non-Elixir tools.

### A taste of the output

Given a resource like:

```elixir
defmodule MyApp.Post do
  use Ash.Resource

  attributes do
    uuid_primary_key :id
    attribute :title, :string, public?: true, allow_nil?: false
    attribute :status, MyApp.Post.Status, public?: true
  end

  relationships do
    belongs_to :author, MyApp.User, public?: true
  end

  actions do
    defaults [:read, create: [:title, :status, :author_id]]
  end
end
```

`Ash.Info.Manifest.generate(otp_app: :my_app)` produces (abridged) roughly:

```elixir
%Ash.Info.Manifest{
  resources: [
    %Ash.Info.Manifest.Resource{
      name: "Post",
      module: MyApp.Post,
      primary_key: [:id],
      fields: %{
        id: %Ash.Info.Manifest.Field{
          name: :id,
          kind: :attribute,
          type: %Ash.Info.Manifest.Type{kind: :uuid, name: "UUID", module: Ash.Type.UUID},
          allow_nil?: false,
          primary_key?: true,
          filter_operators: [
            %Ash.Info.Manifest.ApplicableOperator{name: :==, rhs: :same},
            %Ash.Info.Manifest.ApplicableOperator{name: :in, rhs: {:array, :same}},
            %Ash.Info.Manifest.ApplicableOperator{name: :is_nil,
              rhs: {:concrete, Ash.Type.Boolean}}
          ],
          # ...
        },
        title: %Ash.Info.Manifest.Field{
          name: :title,
          kind: :attribute,
          type: %Ash.Info.Manifest.Type{kind: :string, name: "String", module: Ash.Type.String},
          allow_nil?: false,
          filter_operators: [...],
          filter_functions: [
            %Ash.Info.Manifest.ApplicableFunction{name: :contains,
              rhs: {:concrete, Ash.Type.String}},
            # ...
          ]
        },
        status: %Ash.Info.Manifest.Field{
          name: :status,
          type: %Ash.Info.Manifest.Type{
            kind: :type_ref,    # named-type reference; full def lives in `types`
            name: "Status",
            module: MyApp.Post.Status
          }
        }
      },
      relationships: %{
        author: %Ash.Info.Manifest.Relationship{
          name: :author,
          type: :belongs_to,
          cardinality: :one,
          destination: MyApp.User
        }
      }
    },
    # ... MyApp.User, reached transitively through :author
  ],
  types: [
    %Ash.Info.Manifest.Type{
      kind: :enum,
      name: "Status",
      module: MyApp.Post.Status,
      values: [:draft, :published, :archived]
    }
  ],
  entrypoints: [
    %Ash.Info.Manifest.Entrypoint{
      resource: MyApp.Post,
      action: %Ash.Info.Manifest.Action{
        name: :create,
        type: :create,
        accept: [:title, :status, :author_id],
        arguments: [],
        # ...
      }
    },
    %Ash.Info.Manifest.Entrypoint{
      resource: MyApp.Post,
      action: %Ash.Info.Manifest.Action{name: :read, type: :read, ...}
    }
    # ... entrypoints for MyApp.User
  ],
  filter_capabilities: %Ash.Info.Manifest.FilterCapabilities{
    operators: [%Ash.Info.Manifest.Operator{name: :==, ...}, ...],
    functions: [%Ash.Info.Manifest.Function{name: :contains, ...}, ...],
    predicate_operators: [:==, :!=, :<, :<=, :>, :>=, :in, :is_nil, ...],
    # ...
  },
  sort_capabilities: %Ash.Info.Manifest.SortCapabilities{
    directions: [:asc, :desc, :asc_nils_first, ...]
  }
}
```

Things to notice:

  * The reachable `MyApp.User` appears in `resources` even though we never
    asked for it directly — reachability followed the `:author` relationship.
  * Type references on fields (`%Type{kind: :uuid, module: Ash.Type.UUID}`)
    carry the canonical module. Short-name aliases like `:string` are
    resolved at construction time and never appear in the output.
  * Per-field `filter_operators` / `filter_functions` are already resolved —
    consumers don't re-derive applicability from operator signatures.
  * Named types (like the `:status` enum) are stored as a `:type_ref` on the
    field with the full definition in the top-level `types` list, so a tool
    emitting client code can render the enum once and reference it from
    everywhere.

For a complete overview of the available fields, see `Ash.Info.Manifest`,
`Ash.Info.Manifest.Resource`, `Ash.Info.Manifest.Field`, etc.

## The pattern

Code-generation extensions typically follow this five-step pattern:

1. **Generate the manifest** for the app you're building against.
2. **Verify it has what you need** — fail loudly if a resource is missing a
   required field, an action doesn't exist, etc.
3. **Walk the manifest and apply your DSL's configuration** into the `custom`
   key on each struct. Every manifest struct has a `custom: map()` slot
   specifically for this.
4. **Verify the resulting shape** — your extension's invariants on top of
   Ash's invariants.
5. **Persist** the result. The natural homes are a module attribute (for
   compile-time codegen) or a JSON file (for cross-language tools).

Then either:

  * **Compile** the saved manifest into something else (TypeScript types, a
    GraphQL schema, an OpenAPI document), or
  * **Introspect** the saved manifest at runtime to drive behavior (route
    dispatch, validation, documentation rendering).

The split between "build + verify" and "compile/introspect" is the load-bearing
idea: you do all the cross-checking once, then either path operates against a
pre-validated artifact.

## A worked example

Suppose you're building `MyApp.RpcGen`, an extension that emits a JSON
schema for an RPC interface. Each entrypoint becomes an RPC method, and your
DSL lets the user override the wire name per action:

```elixir
defmodule MyApp.Posts.Post do
  use Ash.Resource, extensions: [MyApp.RpcGen.Resource]

  rpc_gen do
    method :read,   wire_name: "posts.list"
    method :create, wire_name: "posts.create"
  end
end
```

### 1. Generate

```elixir
defmodule MyApp.RpcGen do
  def build!(otp_app) do
    {:ok, manifest} = Ash.Info.Manifest.generate(otp_app: otp_app)
    manifest
  end
end
```

### 2. Verify it has what you need

Walk the manifest and fail loud if an invariant breaks. Don't wait until
compilation downstream — surface the problem at manifest-time:

```elixir
defp validate!(manifest) do
  for entrypoint <- manifest.entrypoints do
    if entrypoint.action.type == :update and entrypoint.action.primary? == false do
      raise "RpcGen: non-primary update action #{inspect(entrypoint.action.name)} " <>
              "on #{inspect(entrypoint.resource)} is not supported"
    end
  end

  manifest
end
```

### 3. Walk and apply your DSL into `custom`

Each entrypoint, resource, field, etc. has a `custom: %{}` map. Populate it
under your extension key:

```elixir
defp apply_config(manifest) do
  entrypoints =
    Enum.map(manifest.entrypoints, fn entrypoint ->
      wire_name =
        MyApp.RpcGen.Info.method_wire_name(
          entrypoint.resource,
          entrypoint.action.name
        ) || default_wire_name(entrypoint)

      put_in(
        entrypoint.action.custom,
        Map.put(entrypoint.action.custom, :rpc_gen, %{wire_name: wire_name})
      )
      |> then(&%{entrypoint | action: &1})
    end)

  %{manifest | entrypoints: entrypoints}
end
```

After this step, every action carries its RPC config alongside its Ash config,
in the same struct. Downstream code never re-reads the DSL.

### 4. Verify the resulting shape

Your extension's invariants on top of Ash's. For an RPC system, wire names
must be unique:

```elixir
defp validate_wire_names!(manifest) do
  manifest.entrypoints
  |> Enum.group_by(& &1.action.custom.rpc_gen.wire_name)
  |> Enum.each(fn
    {_, [_]} -> :ok
    {wire_name, dupes} ->
      raise "RpcGen: wire_name #{inspect(wire_name)} used by multiple actions: " <>
              inspect(Enum.map(dupes, &{&1.resource, &1.action.name}))
  end)
end
```

### 5. Persist

For a compile-time extension, store the validated manifest in a module
attribute. `Spark.Dsl.Extension` transformers run at compile time, so this is
free:

```elixir
defmodule MyApp.RpcGen.Schema do
  @manifest Macro.escape(MyApp.RpcGen.build_validated!(:my_app))

  def manifest, do: @manifest

  def wire_names do
    Enum.map(@manifest.entrypoints, & &1.action.custom.rpc_gen.wire_name)
  end
end
```

For a runtime/CLI generator, write JSON to disk:

```elixir
manifest
|> Ash.Info.Manifest.JsonSerializer.to_map()
|> Jason.encode!(pretty: true)
|> then(&File.write!("priv/rpc_schema.json", &1))
```

### Compile or introspect

From here, you have two choices. To **compile** the manifest into a target
language, walk it and emit:

```elixir
def emit_typescript(manifest) do
  for resource <- manifest.resources, into: "" do
    """
    export interface #{resource.name} {
    #{Enum.map_join(Map.values(resource.fields), "\n", &emit_field/1)}
    }
    """
  end
end
```

To **introspect** at runtime — for a dispatcher, a docs page, a request
validator — query the same persisted manifest:

```elixir
def dispatch(wire_name, payload) do
  entrypoint =
    Enum.find(@manifest.entrypoints, fn e ->
      e.action.custom.rpc_gen.wire_name == wire_name
    end)

  case entrypoint do
    nil -> {:error, :unknown_method}
    %{resource: resource, action: action} -> run(resource, action, payload)
  end
end
```

## What goes in `custom`

The `custom` slot is intentionally untyped. Convention:

  * Namespace under your extension's key: `entrypoint.action.custom.ash_graphql`,
    not `entrypoint.action.custom.queryable?`. Extensions don't conflict.
  * Treat it as data, not behavior — store strings, atoms, booleans, maps.
    Don't store functions or PIDs; they don't serialize and prevent
    persistence to JSON.
  * Symmetric structures get symmetric keys. If you put RPC config on
    `entrypoint.action.custom.rpc_gen`, put per-field RPC config on
    `field.custom.rpc_gen`.

## Visibility options

By default the manifest only contains items marked `public?: true`. For most
extensions this is what you want — private fields and actions are internal
implementation. If your extension *does* need to see private items (e.g. a
documentation generator that should emit internal-only fields under a
separate header), opt in explicitly:

```elixir
Ash.Info.Manifest.generate(
  otp_app: :my_app,
  include_private_attributes?: true,
  include_private_calculations?: true,
  include_private_aggregates?: true,
  include_private_relationships?: true,
  include_private_arguments?: true,
  include_private_actions?: true
)
```

The defaults exist so generated artifacts can't accidentally leak intentionally
private API surface.

## Filter and sort capabilities

`manifest.filter_capabilities` carries every operator, function, and custom
expression in the app. `manifest.sort_capabilities` carries the sort
directions. These are the universe — what's actually applicable to a *field*
is on the field itself:

```elixir
field = Ash.Info.Manifest.get_field(resource_lookup, MyApp.Post, :title)
field.filter_operators
# => [%ApplicableOperator{name: :==, rhs: :same}, ..., %ApplicableOperator{name: :is_nil, rhs: {:concrete, Ash.Type.Boolean}}]
field.filter_functions
# => [%ApplicableFunction{name: :contains, rhs: {:concrete, Ash.Type.String}}, ...]
```

The `rhs` tells you the right-hand-side type. `:same` means the same type as
the field; `{:concrete, module}` is a specific Ash type module (always a
module — never a short-name alias like `:string`); `{:array, rhs}` is an
array whose element follows the nested shape. Consumers can render these
directly without re-deriving from operator signatures.

## Why module references, not short-name atoms

Throughout the manifest, type references are Ash type modules
(`Ash.Type.String`) rather than short-name atoms (`:string`). Short names are
user-facing aliases — users register `:slug` to point at `MyApp.Types.Slug` in
their config — so emitting them in the manifest would leak a per-app naming
convention that consumers can't reliably interpret. The manifest resolves
short names through `Ash.Type.get_type/1` at construction time so consumers
always work against canonical modules.

If you want to display a friendly short name in generated output, look up the
mapping yourself from `Ash.Type.short_names/0` — that keeps the choice of
display name local to your extension rather than baked into the IR.

## Performance

`Ash.Info.Manifest.generate/1` walks every reachable resource and type, which
is non-trivial for large apps. The intended usage is:

  * **Compile-time extensions**: call it once during compilation, persist the
    result in a module attribute. Subsequent module attribute reads are free.
  * **Runtime/CLI tools**: call it once at boot, hold the manifest in a process
    or `:persistent_term`.

Don't call `generate/1` per-request.

## Versioning

`Ash.Info.Manifest.schema_version/0` returns the schema version of the JSON
output. Cross-language consumers should record this and refuse incompatible
versions. The version bumps when the JSON shape changes in a way downstream
tools need to detect; pure Elixir consumers reading the structs typically
don't need to check.

## See also

  * `Ash.Info.Manifest` — the manifest struct and top-level API.
  * `Ash.Info.Manifest.Resource`, `.Field`, `.Relationship`, `.Action`,
    `.Type`, `.Operator`, `.Function`, `.CustomExpression` — the individual
    struct definitions.
  * `Ash.Info.Manifest.JsonSerializer` — JSON serialization for cross-language
    tooling.
  * [Writing Extensions](writing-extensions.md) — for the DSL-side
    machinery that you pair with the manifest walker.
