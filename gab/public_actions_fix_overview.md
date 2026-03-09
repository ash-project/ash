## Ash internal-only actions: `public?` and `private_action?`

This document explains the **issue**, the **feature** we implemented, and **all file-level changes** related to the new `public?` option on actions and the `private_action?()` policy check. It is intended as a design + implementation note for reviewers and future maintainers.

Created using AALang and Gab.

---

### 1. Problem statement

In complex systems you often have **actions that should only be used internally**, for example:

- a low-level **lookup** action (`get_user_by_secret_token`)
- a **helper create/update** action used from other actions, but never meant to be exposed through:
  - JSON:API (AshJsonApi)
  - GraphQL (AshGraphql)
  - TypeScript clients (AshTypescript)
  - UI forms (AshPhoenix)

Before this change there were two imperfect workarounds:

- **Policies + `authorize_if always()`**  
  You could write a policy that always authorizes an internal-only action. However, that action still looks “public” to API extensions and may accidentally be exposed (e.g. someone wires it into GraphQL).

- **`authorized?: false`**  
  You could bypass authorization entirely, but then you **lose the authorization context** (actor, tenant, nested actions), breaking the authorization “circuit” and making it harder to reason about security for sub-actions.

**Goal:**  
Provide a **first-class `public?` flag on actions** and a **policy check** that lets us safely bypass authorization *only* for actions that are truly internal, while giving API extensions a clear signal that they **must not expose** these actions.

---

### 2. New behavior in one example

#### Example resource

```elixir
defmodule MyApp.Accounts.User do
  use Ash.Resource,
    domain: MyApp.Accounts

  actions do
    # Public read action – should be exposed to APIs
    read :read do
      primary? true
    end

    # Internal helper: used only by other actions in the system
    read :lookup_by_token do
      public? false

      argument :token, :string, allow_nil?: false

      filter expr(reset_token == ^arg(:token))
    end
  end

  policies do
    policy bypass private_action?() do
      # Internal-only actions are allowed, but only because they are guaranteed
      # NOT to be exposed by public interfaces.
      authorize_if always()
    end

    # other policies for public actions...
  end
end
```

- API extensions should now call **`Ash.Resource.Info.public_actions(User)`** (or equivalent) and will **see only actions with `public?: true`** (e.g. `:read`, but **not** `:lookup_by_token`).
- Policies can safely say:

  ```elixir
  policy bypass private_action?() do
    authorize_if always()
  end
  ```

  because by construction no public API should ever route to a `public?: false` action.

---

### 3. Core implementation changes

Below are the **core code files** changed for the feature, what each file does, and what exactly we changed.

---

#### 3.1 `lib/ash/resource/actions/shared_options.ex`

**What the file does**

Defines **shared option schemas** for all action types (create, read, update, destroy, and generic actions). These options are reused across action modules via `shared_options/0` and `create_update_opts/0`.

**What we added**

- New option **`public?`** in `@shared_options`:

  ```elixir
  public?: [
    type: :boolean,
    default: true,
    doc: """
    Whether the action is part of the resource's public API. When `false`, the action is internal-only
    and must not be exposed by API extensions (e.g. AshGraphql, AshJsonApi). Use `bypass private_action?() do
    authorize_if always() end` in policies to allow internal callers. Defaults to `true`.
    """
  ]
  ```

**Effect**

- All action DSLs (create/read/update/destroy/action) now accept `public?` in their blocks.
- The behavior **defaults to `public?: true`**, preserving existing semantics unless explicitly marked internal.

---

#### 3.2 `lib/ash/resource/actions/action/action.ex`

**What the file does**

Defines the **struct and options** for generic “`action`” actions, i.e. `actions do action :name, ... end`.

**What we changed**

- Added a **`public?: true`** field to the action struct:

  ```elixir
  defstruct [
    :name,
    :description,
    :returns,
    :run,
    constraints: [],
    touches_resources: [],
    skip_unknown_inputs: [],
    arguments: [],
    preparations: [],
    allow_nil?: false,
    transaction?: false,
    primary?: false,
    skip_global_validations?: false,
    public?: true,
    type: :action,
    __spark_metadata__: nil
  ]
  ```

- Updated the **type spec** to include `public?: boolean`:

  ```elixir
  @type t :: %__MODULE__{
          ...
          primary?: boolean,
          transaction?: boolean,
          public?: boolean,
          ...
        }
  ```

**Effect**

- Generic actions now carry a **strongly-typed `public?` flag** that downstream code (extensions, policies, tooling) can rely on.

---

#### 3.3 `lib/ash/resource/actions/create.ex`
#### 3.4 `lib/ash/resource/actions/read.ex`
#### 3.5 `lib/ash/resource/actions/update.ex`
#### 3.6 `lib/ash/resource/actions/destroy.ex`

**What these files do**

Each module defines the **struct, option schema, and transform logic** for its specific action type:

- `Create` – create actions
- `Read` – read actions
- `Update` – update actions
- `Destroy` – destroy actions

**What we changed**

For each struct we added a `public?: true` field, and for each type spec we added `public?: boolean`.

Examples (pattern is the same in each file):

```elixir
defstruct [
  :name,
  :primary?,
  :description,
  :error_handler,
  :multitenancy,
  ...
  transaction?: true,
  public?: true,
  type: :create,
  __spark_metadata__: nil
]
```

and:

```elixir
@type t :: %__MODULE__{
        ...
        primary?: boolean,
        public?: boolean,
        ...
      }
```

**Effect**

- **All first-class action types** now store `public?` in their struct:
  - `Ash.Resource.Actions.Create.t`
  - `Ash.Resource.Actions.Read.t`
  - `Ash.Resource.Actions.Update.t`
  - `Ash.Resource.Actions.Destroy.t`
  - `Ash.Resource.Actions.Action.t` (generic)
- This ensures **consistent behavior** and makes it easy to treat all actions uniformly when filtering by `public?`.

---

#### 3.7 `lib/ash/resource/info.ex`

**What the file does**

`Ash.Resource.Info` is the **introspection module** for resources. It exposes functions like:

- `attributes/1`
- `relationships/1`
- `actions/1`
- `public_attributes/1`, etc.

**What we added**

- A new function **`public_actions/1`**:

  ```elixir
  @doc "Returns all public actions of a resource (actions with `public?: true`). Use for building public APIs; do not expose actions with `public?: false`."
  @spec public_actions(Spark.Dsl.t() | Ash.Resource.t()) :: [Ash.Resource.Actions.action()]
  def public_actions(resource) do
    resource
    |> actions()
    |> Enum.filter(& &1.public?)
  end
  ```

**Effect**

- API extensions (AshGraphql, AshJsonApi, AshTypescript, AshPhoenix, etc.) now have a **single, explicit API** for:
  - “Give me only the actions that should be public.”
  - They should **not** consider actions with `public?: false` when generating public interfaces.

---

#### 3.8 `lib/ash/policy/check/private_action.ex`

**What the file does**

Defines a new **policy check** used in Ash policies:

```elixir
defmodule Ash.Policy.Check.PrivateAction do
  @moduledoc """
  This check is true when the current action is private (`public?: false`).

  Use it to bypass authorization for internal-only actions that must never be
  exposed by API extensions (e.g. AshGraphql, AshJsonApi):

  ```elixir
  policy bypass private_action?() do
    authorize_if always()
  end
  ```
  """
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(_) do
    "action is private (public?: false)"
  end

  @impl true
  def match?(_actor, %{action: action}, _opts) do
    # Default to true (public) when key is missing for backwards compatibility
    !Map.get(action, :public?, true)
  end
end
```

**Effect**

- Policies can now **safely express**:

  ```elixir
  policy bypass private_action?() do
    authorize_if always()
  end
  ```

  knowing that:

  - It only matches **`public?: false`** actions.
  - No API extension should route user requests to those actions.
  - Existing resources that do not have a `public?` field still behave as if they are `public?: true` (backwards compatible via `Map.get(action, :public?, true)`).

---

#### 3.9 `lib/ash/policy/check/built_in_checks.ex`

**What the file does**

Provides **built-in policy check helpers**, such as:

- `always/0`, `never/0`
- `action_type/1`
- `resource/1`
- `actor_present/0`, `actor_absent/0`
- and many more.

**What we added**

- New helper **`private_action?/0`**:

  ```elixir
  @doc """
  This check is true when the current action is private (`public?: false`).

  Use it to bypass authorization for internal-only actions that must never be exposed
  by API extensions:

  ```elixir
  policy bypass private_action?() do
    authorize_if always()
  end
  ```
  """
  @spec private_action?() :: Ash.Policy.Check.ref()
  def private_action? do
    Ash.Policy.Check.PrivateAction
  end
  ```

**Effect**

- Users write:

  ```elixir
  import Ash.Policy.Check.Builtins

  policies do
    policy bypass private_action?() do
      authorize_if always()
    end
  end
  ```

  instead of referencing `Ash.Policy.Check.PrivateAction` directly.

---

#### 3.10 `lib/ash/policy/policy.ex`

**What the file does**

Implements the **policy engine**, including:

- Representation of policies
- Combining checks
- Optimization (conflict detection, simplification)
- **Check priority ordering** for scenarios in Crux.

**What we changed**

- We **registered the new `PrivateAction` check** in the `@check_priorities` list so that:
  - The Crux scenario sorter knows about it.
  - It participates correctly in simplification and conflict detection.

Snippet (the relevant fragment):

```elixir
@check_priorities [
  Ash.Policy.Check.Static,
  Ash.Policy.Check.Action,
  Ash.Policy.Check.ActionType,
  Ash.Policy.Check.PrivateAction,
  Ash.Policy.Check.ActorAbsent,
  Ash.Policy.Check.ActorPresent
]
|> Enum.with_index()
|> Map.new()
```

**Effect**

- `PrivateAction` is now a **first-class check** in the policy engine, ordered alongside other core checks.

---

### 4. Test change

#### 4.1 `test/policy/policy_test.exs`

**What the file does**

Contains tests for `Ash.Policy.Policy`, including:

- Behavior of policy combination.
- Behavior of built-in checks.
- Behavior of `debug_expr/2`, which converts a policy expression into a human-readable string.

**What we changed**

- On Windows the test for `debug_expr/2` failed because the expected string used `\r\n` line endings while the actual output used `\n`. We adjusted the test to **normalize line endings** before comparison:

```elixir
describe inspect(&Ash.Policy.Policy.debug_expr/2) do
  test "generates readable expression" do
    result =
      Ash.Policy.Policy.debug_expr(
        b({RuntimeCheck, []} and ({RuntimeCheck, [some: :config]} or not true))
      )

    # Normalize line endings so test passes on both Unix and Windows
    expected =
      "Expr:\n\n\"returns true at runtime\" and (\"returns true at runtime\" or not true)"

    assert String.replace(result, "\r\n", "\n") == expected
  end
end
```

**Effect**

- `mix test test/policy/` now passes on both **Unix and Windows**, which is important for validating our new policy check via `mix ash.verify_public_actions`.

---

### 5. Documentation regeneration

The following **generated DSL docs** were also updated as a side-effect of running the doc generator (e.g. `mix docs`) after adding the new feature:

- `documentation/dsls/DSL-Ash.Resource.md`
- `documentation/dsls/DSL-Ash.Domain.md`
- `documentation/dsls/DSL-Ash.Policy.Authorizer.md`
- `documentation/dsls/DSL-Ash.Notifier.PubSub.md`
- `documentation/dsls/DSL-Ash.DataLayer.Ets.md`
- `documentation/dsls/DSL-Ash.DataLayer.Mnesia.md`
- `documentation/dsls/DSL-Ash.Reactor.md`
- `documentation/dsls/DSL-Ash.TypedStruct.md`

**What they do**

- These are auto-generated **reference docs** for Ash DSLs. They are not hand-edited.

**What changed conceptually**

- `DSL-Ash.Resource.md` and `DSL-Ash.Policy.Authorizer.md` now **document the new configuration knobs** (e.g. `public?` on actions, visibility of checks).
- The other files were regenerated as part of the same run and should be considered **documentation refresh** rather than feature logic.

---

### 6. Non-feature extras (not required for upstream PR)

For completeness, there are a few local/developer-oriented additions that support or exercise the feature but are **not strictly part of the core change**:

- `lib/mix/tasks/ash.verify_public_actions.ex`  
  - Mix task that runs the curated verification tests (policy + resource info + resource actions) and exits 0 only if they all pass.
- `lib/mix/tasks/check.reuse.ex`  
  - Local workaround to make the `reuse` tool in `mix check` more forgiving when `pipx` is missing.
- `gab/ash-public-actions-test-agent.jsonld` and `gab/README.md`  
  - A GAB agent and readme that **run the verification flow** via `mix ash.verify_public_actions`.

These are useful for local workflows and automated agents, but the **minimal feature set** for a PR into the main Ash repo is:

1. All changes in section **3** (core implementation).
2. The test change in **4.1**.
3. The regenerated docs in **5**, if the project expects docs to be updated alongside code.

---

Created using AALang and Gab.

