# Rules for working with Ash

## Understanding Ash

Ash is an opinionated, composable framework for building applications in Elixir. It provides a declarative approach to modeling your domain with resources at the center. Read documentation  *before* attempting to use it's features. Do not assume that you have prior knowledge of the framework or its conventions.

## Code Structure & Organization

- Organize code around domains and resources
- Each resource should be focused and well-named
- Create domain-specific actions rather than generic CRUD operations
- Put business logic inside actions rather than in external modules
- Use resources to model your domain entities

## Code Interfaces

Use code interfaces on domains to define the contract for calling into Ash resources. See the [Code interface guide for more](https://hexdocs.pm/ash/code-interfaces.html/).

Define code interfaces on the domain, like this:

```elixir
resource ResourceName do
  define :fun_name, action: :action_name
end
```

For more complex interfaces with custom transformations:

```elixir
define :custom_action do
  action :action_name
  args [:arg1, :arg2]

  custom_input :arg1, MyType do
    transform do
      to :target_field
      using &MyModule.transform_function/1
    end
  end
end
```

## Actions

- Create specific, well-named actions rather than generic ones
- Put all business logic inside action definitions
- Use hooks like `Ash.Changeset.after_action/2`, `Ash.Changeset.before_action/2` to add additional logic
  inside the same transaction.
- Use hooks like `Ash.Changeset.after_transaction/2`, `Ash.Changeset.before_transaction/2` to add additional logic
  inside the same transaction.
- Use action arguments for inputs that need validation
- Use preparations to modify queries before execution
- Use changes to modify changesets before execution
- Use validations to validate changesets before execution
- Prefer domain code interfaces to call actions instead of directly building queries/changesets and calling functions in the `Ash` module
- A resource could be *only generic actions*. This can be useful when you are using a resource only to model behavior.

## Error Handling

Functions to call actions, like `Ash.create` and code interfaces like `MyApp.Accounts.register_user` all return ok/error tuples. All have `!` variations, like `Ash.create!` and `MyApp.Accounts.register_user!`. Use the `!` variations when you want to "let it crash", like if looking something up that should definitely exist, or calling an action that should always succeed.

All Ash code returns errors in the form of `{:error, error_class}`. Ash categorizes errors into four main classes:

1. **Forbidden** (`Ash.Error.Forbidden`) - Occurs when a user attempts an action they don't have permission to perform
2. **Invalid** (`Ash.Error.Invalid`) - Occurs when input data doesn't meet validation requirements
3. **Framework** (`Ash.Error.Framework`) - Occurs when there's an issue with how Ash is being used
4. **Unknown** (`Ash.Error.Unknown`) - Occurs for unexpected errors that don't fit the other categories

These error classes help you catch and handle errors at an appropriate level of granularity. An error class will always be the "worst" (highest in the above list) error class from above. Each error class can contain multiple underlying errors, accessible via the `errors` field on the exception.

### Using Validations

Validations ensure that data meets your business requirements before it gets processed by an action. Unlike changes, validations cannot modify the changeset - they can only validate it or add errors.

- Use **built-in validations** (defined in `Ash.Resource.Validation.Builtins`) for common validation patterns:
  ```elixir
  validate match(:email, ~r/@/)
  validate compare(:age, greater_than_or_equal_to: 18)
  validate present(:first_name)
  validate one_of(:status, [:active, :inactive, :pending])
  ```

- Add **custom error messages** to make validation errors user-friendly:
  ```elixir
  validate compare(:age, greater_than_or_equal_to: 18) do
    message "You must be at least 18 years old to sign up"
  end
  ```

- Apply validations **conditionally** using `where`:
  ```elixir
  validate present(:phone_number) do
    where present(:contact_method)
    where eq(:contact_method, "phone")
    message "Phone number is required when phone is selected as contact method"
  end
  ```

- Create **action-specific validations** inside your action definitions:
  ```elixir
  actions do
    create :sign_up do
      validate present([:email, :password])
      validate match(:email, ~r/@/)
    end
  end
  ```

- Add **global validations** that apply to multiple action types:
  ```elixir
  validations do
    validate present([:title, :body]), on: [:create, :update]
    validate absent(:admin_note), on: :create, where: not_actor_attribute(:is_admin, true)
  end
  ```

- Create **custom validation modules** for complex validation logic:
  ```elixir
  defmodule MyApp.Validations.UniqueUsername do
    use Ash.Resource.Validation

    @impl true
    def init(opts), do: {:ok, opts}

    @impl true
    def validate(changeset, _opts, _context) do
      # Validation logic here
      # Return :ok or {:error, message}
    end
  end

  # Usage in resource:
  validate {MyApp.Validations.UniqueUsername, []}
  ```

- Make validations **atomic** when possible to ensure they work correctly with direct database operations by implementing the `atomic/3` callback in custom validation modules.

```elixir
defmodule MyApp.Validations.IsEven do
  # transform and validate opts

  use Ash.Resource.Validation

  @impl true
  def init(opts) do
    if is_atom(opts[:attribute]) do
      {:ok, opts}
    else
      {:error, "attribute must be an atom!"}
    end
  end

  @impl true
  # This is optional, but useful to have in addition to validation
  # so you get early feedback for validations that can otherwise
  # only run in the datalayer
  def validate(changeset, opts, _context) do
    value = Ash.Changeset.get_attribute(changeset, opts[:attribute])

    if is_nil(value) || (is_number(value) && rem(value, 2) == 0) do
      :ok
    else
      {:error, field: opts[:attribute], message: "must be an even number"}
    end
  end

  @impl true
  def atomic(changeset, opts, context) do
    {:atomic,
      # the list of attributes that are involved in the validation
      [opts[:attribute]],
      # the condition that should cause the error
      # here we refer to the new value or the current value
      expr(rem(^atomic_ref(opts[:attribute]), 2) != 0),
      # the error expression
      expr(
        error(^InvalidAttribute, %{
          field: ^opts[:attribute],
          # the value that caused the error
          value: ^atomic_ref(opts[:attribute]),
          # the message to display
          message: ^(context.message || "%{field} must be an even number"),
          vars: %{field: ^opts[:attribute]}
        })
      )
    }
  end
end
```

### Using Changes

Changes allow you to modify the changeset before it gets processed by an action. Unlike validations, changes can manipulate attribute values, add attributes, or perform other data transformations.

- Use **built-in changes** (defined in `Ash.Resource.Change.Builtins`) for common transformation patterns:
```elixir
change set_attribute(:status, "pending")
change relate_actor(:user)
change atomic_update(:counter, expr(^counter + 1))
```

- Apply changes **conditionally** using `where`:
```elixir
change set_attribute(:paid, true) do
  where attribute_equals(:payment_method, "free")
end

change relate_actor(:creator) do
  where present(:actor)
end
```

- Create **action-specific changes** inside your action definitions:
```elixir
actions do
  create :sign_up do
    change set_attribute(:joined_at, expr(now()))
    change increment(:visit_count)
    change optimistic_lock(:version)
    change debug_log("Processing signup")
  end
end
```

- Add **global changes** that apply to multiple action types:
```elixir
changes do
  change set_attribute(:updated_at, expr(now())), on: :update
  change set_attribute(:inserted_at, expr(now())), on: :create
  change prevent_change(:secret_field), on: [:update, :destroy]
  change manage_relationship(:related_items, :items), on: [:create, :update]
end
```

- Create **custom change modules** for reusable transformation logic:
  ```elixir
  defmodule MyApp.Changes.SlugifyTitle do
    use Ash.Resource.Change

    def change(changeset, _opts, _context) do
      title = Ash.Changeset.get_attribute(changeset, :title)

      if title do
        slug = title |> String.downcase() |> String.replace(~r/[^a-z0-9]+/, "-")
        Ash.Changeset.change_attribute(changeset, :slug, slug)
      else
        changeset
      end
    end
  end

  # Usage in resource:
  change {MyApp.Changes.SlugifyTitle, []}
  ```

- Create a **change module with lifecycle hooks** to handle complex multi-step operations:

```elixir
defmodule MyApp.Changes.ProcessOrder do
  use Ash.Resource.Change

  def change(changeset, _opts, _context) do
    changeset
    |> Ash.Changeset.before_transaction(fn changeset ->
      # Runs before the transaction starts
      # Use for external API calls, logging, etc.
      MyApp.ExternalService.reserve_inventory(changeset)
      changeset
    end)
    |> Ash.Changeset.before_action(fn changeset ->
      # Runs inside the transaction before the main action
      # Use for related database changes in the same transaction
      Ash.Changeset.change_attribute(changeset, :processed_at, DateTime.utc_now())
    end)
    |> Ash.Changeset.after_action(fn changeset, result ->
      # Runs inside the transaction after the main action, only on success
      # Use for related database changes that depend on the result
      MyApp.Inventory.update_stock_levels(result)
      {changeset, result}
    end)
    |> Ash.Changeset.after_transaction(fn changeset,
      {:ok, result} ->
        # Runs after the transaction completes (success or failure)
        # Use for notifications, external systems, etc.
        MyApp.Mailer.send_order_confirmation(result)
        {changeset, result}

      {:error, error} ->
        # Runs after the transaction completes (success or failure)
        # Use for notifications, external systems, etc.
        MyApp.Mailer.send_order_issue_notice(result)
        {:error, error}
    end)
  end
end

# Usage in resource:
change {MyApp.Changes.ProcessOrder, []}
```

## Anonymous Functions

Prefer to put code in its own module and refer to that in changes, preparations, validations etc.

For example, prefer this:

```elixir
# in
defmodule MyApp.MyDomain.MyResource.Changes.SlugifyName do
  use Ash.Resource.Change

  def change(changeset, _, _) do
    Ash.Changeset.before_action(changeset, fn changeset, _ ->
      slug = MyApp.Slug.get()
      Ash.Changeset.force_change_attribute(changeset, :slug, slug)
    end)
  end
end

change MyApp.MyDomain.MyResource.Changes.SlugifyName
```

### Action Types

- **Read**: For retrieving records
- **Create**: For creating records
- **Update**: For changing records
- **Destroy**: For removing records
- **Generic**: For custom operations that don't fit the other types

## Relationships

Relationships describe connections between resources and are a core component of Ash. Define relationships in the `relationships` block of a resource.

### Best Practices for Relationships

- Be descriptive with relationship names (e.g., use `:authored_posts` instead of just `:posts`)
- Configure foreign key constraints in your data layer if they have them (see `references` in AshPostgres)
- Always choose the appropriate relationship type based on your domain model

### Types of Relationships

#### belongs_to

Use when a resource "belongs to" another resource. This adds a foreign key to the source resource.

```elixir
relationships do
  belongs_to :owner, MyApp.User do
    # Customize the foreign key attribute (defaults to :owner_id)
    source_attribute :custom_name

    # Customize the type (defaults to :uuid)
    attribute_type :integer

    # Control whether the attribute is public
    attribute_public? true

    # Set constraints on the relationship
    allow_nil? false
    primary_key? false
  end
end
```

#### has_one

Use when a resource "has one" of another resource. The foreign key is on the destination resource.

```elixir
relationships do
  has_one :profile, MyApp.Profile do
    # These are typically used with defaults
    source_attribute :id  # Default
    destination_attribute :user_id  # Default is <resource_name>_id
  end
end
```

#### has_many

Use when a resource "has many" of another resource. The foreign key is on the destination resource.

```elixir
relationships do
  has_many :posts, MyApp.Post do
    # Similar to has_one but returns a list of related records
    source_attribute :id  # Default
    destination_attribute :user_id  # Default is <resource_name>_id

    # Filter the related records
    filter expr(published == true)

    # Sort the related records
    sort published_at: :desc
  end
end
```

#### many_to_many

Use when many resources can be related to many other resources. Requires a join resource.

```elixir
relationships do
  many_to_many :tags, MyApp.Tag do
    through MyApp.PostTag
    source_attribute_on_join_resource :post_id
    destination_attribute_on_join_resource :tag_id
  end
end
```

The join resource must be defined separately:

```elixir
defmodule MyApp.PostTag do
  use Ash.Resource,
    data_layer: AshPostgres.DataLayer

  attributes do
    uuid_primary_key :id
    # Add additional attributes if you need metadata on the relationship
    attribute :added_at, :utc_datetime_usec do
      default &DateTime.utc_now/0
    end
  end

  relationships do
    belongs_to :post, MyApp.Post, primary_key?: true, allow_nil?: false
    belongs_to :tag, MyApp.Tag, primary_key?: true, allow_nil?: false
  end

  actions do
    defaults [:read, :destroy, create: :*, update: :*]
  end
end
```

### Loading Relationships

Load relationships either in a query or directly on records:

```elixir
# In a query
MyApp.Post
|> Ash.Query.load(:author)
|> Ash.Query.load(comments: [:author])
|> MyDomain.read!()

# On records
post = MyDomain.get_post!(id)
post_with_author = Ash.load!(post, :author)

# Complex loading with customized queries
MyApp.Post
|> Ash.Query.load(comments:
  MyApp.Comment
  |> Ash.Query.filter(is_approved == true)
  |> Ash.Query.sort(created_at: :desc)
  |> Ash.Query.limit(5)
)
|> MyDomain.read!()
```

Prefer to use the `strict?` option when loading to only load necessary fields on related data.

```Elixir
MyApp.Post
|> Ash.Query.load([comments: [:title]], strict?: true)
```

### Managing Relationships

Use `manage_relationship` to handle related data in actions:

```elixir
actions do
  update :update do
    # Define argument for the related data
    argument :comments, {:array, :map} do
      allow_nil? false
    end

    argument :new_tags, {:array, :map}

    # Link argument to relationship management
    change manage_relationship(:comments, type: :append)

    # For different argument and relationship names
    argument :new_tags, {:array, :map}
    change manage_relationship(:new_tags, :tags, type: :append)
  end
end
```

#### Built in relationship management types

- `:create` - Create new related records
- `:append` - Add existing records to the relationship
- `:remove` - Remove specific related records from the relationship
- `:append_and_remove` - Add related records from the relationship, removing any not provided.
- `:direct_control` - Fully replace all related records with the provided data, creating anything new, deleting anything not provided, and updating any existing records.

#### Practical Examples

Creating a post with tags:
```elixir
MyDomain.create_post!(%{
  title: "New Post",
  body: "Content here...",
  tags: [%{name: "elixir"}, %{name: "ash"}]  # Creates new tags
})

# Updating a post to replace its tags
MyDomain.update_post!(post, %{
  tags: [tag1.id, tag2.id]  # Replaces tags with existing ones by ID
})
```

## Generating Code

Use `mix ash.gen.*` tasks as a basis for code generation when possible. Check the task docs with `mix help <task>`.
Be sure to use `--yes` to bypass confirmation prompts. Use `--yes --dry-run` to preview the changes.

## Data Layers

Data layers determine how resources are stored and retrieved. Examples of data layers:

- **Postgres**: For storing resources in PostgreSQL (via `AshPostgres`)
- **ETS**: For in-memory storage (`Ash.DataLayer.Ets`)
- **Mnesia**: For distributed storage (`Ash.DataLayer.Mnesia`)
- **Embedded**: For resources embedded in other resources (`data_layer: :embedded`) (typically JSON under the hood)
- **Ash.DataLayer.Simple**: For resources that aren't persisted at all. Leave off the data layer, as this is the default.

Specify a data layer when defining a resource:

```elixir
defmodule MyApp.Post do
  use Ash.Resource,
    domain: MyApp.Blog,
    data_layer: AshPostgres.DataLayer

  postgres do
    table "posts"
    repo MyApp.Repo
  end

  # ... attributes, relationships, etc.
end
```

For embedded resources:

```elixir
defmodule MyApp.Address do
  use Ash.Resource,
    data_layer: :embedded

  attributes do
    attribute :street, :string
    attribute :city, :string
    attribute :state, :string
    attribute :zip, :string
  end
end
```

Each data layer has its own configuration options and capabilities. Refer to the rules & documentation of the specific data layer package for more details.

## Migrations and Schema Changes

After creating or modifying Ash code, run `mix ash.codegen <short_name_describing_changes>` to ensure any required additional changes are made (like migrations are generated).

## Authorization

- When performing administrative actions, you can bypass authorization with `authorize?: false`
- To run actions as a particular user, look that user up and pass it as the `actor` option
- Always set the actor on the query/changeset/input, not when calling the action
- Use policies to define authorization rules

```elixir
# Good
Post
|> Ash.Query.for_read(:read, %{}, actor: current_user)
|> Ash.read!()
```

### Policies

To use policies, add the `Ash.Policy.Authorizer` to your resource:

```elixir
defmodule MyApp.Post do
  use Ash.Resource,
    domain: MyApp.Blog,
    authorizers: [Ash.Policy.Authorizer]

  # Rest of resource definition...
end
```

### Policy Basics

Policies determine what actions on a resource are permitted for a given actor. Define policies in the `policies` block:

```elixir
policies do
  # A simple policy that applies to all read actions
  policy action_type(:read) do
    # Authorize if record is public
    authorize_if expr(public == true)

    # Authorize if actor is the owner
    authorize_if relates_to_actor_via(:owner)
  end

  # A policy for create actions
  policy action_type(:create) do
    # Only allow active users to create records
    forbid_unless actor_attribute_equals(:active, true)

    # Ensure the record being created relates to the actor
    authorize_if relating_to_actor(:owner)
  end
end
```

### Policy Evaluation Flow

Policies evaluate from top to bottom with the following logic:

1. All policies that apply to an action must pass for the action to be allowed
2. Within each policy, checks evaluate from top to bottom
3. The first check that produces a decision determines the policy result
4. If no check produces a decision, the policy defaults to forbidden

### Bypass Policies

Use bypass policies to allow certain actors to bypass other policy restrictions. This should be used almost exclusively for admin bypasses.

```elixir
policies do
  # Bypass policy for admins - if this passes, other policies don't need to pass
  bypass actor_attribute_equals(:admin, true) do
    authorize_if always()
  end

  # Regular policies follow...
  policy action_type(:read) do
    # ...
  end
end
```

### Field Policies

Field policies control access to specific fields (attributes, calculations, aggregates):

```elixir
field_policies do
  # Only supervisors can see the salary field
  field_policy :salary do
    authorize_if actor_attribute_equals(:role, :supervisor)
  end

  # Allow access to all other fields
  field_policy :* do
    authorize_if always()
  end
end
```

### Policy Checks

There are two main types of checks used in policies:

1. **Simple checks** - Return true/false answers (e.g., "is the actor an admin?")
2. **Filter checks** - Return filters to apply to data (e.g., "only show records owned by the actor")

You can use built-in checks or create custom ones:

```elixir
# Built-in checks
authorize_if actor_attribute_equals(:role, :admin)
authorize_if relates_to_actor_via(:owner)
authorize_if expr(public == true)

# Custom check module
authorize_if MyApp.Checks.ActorHasPermission
```

#### Custom Simple Check Example

Create a custom simple check by implementing `Ash.Policy.SimpleCheck`:

```elixir
defmodule MyApp.Checks.ActorHasRequiredRole do
  use Ash.Policy.SimpleCheck

  # Provide a description for logging and debugging
  def describe(opts) do
    "actor has required role: #{opts[:role] || "admin"}"
  end

  # Implement the check logic - must return true or false
  def match?(%{role: actor_role} = _actor, _context, opts) do
    required_role = opts[:role] || :admin
    actor_role == required_role
  end

  # Handle case when actor doesn't have role attribute
  def match?(_, _, _), do: false
end

# Usage in policies
policy action_type(:read) do
  # Pass options to the check
  authorize_if {MyApp.Checks.ActorHasRequiredRole, role: :manager}
end
```

#### Custom Filter Check Example

Create a custom filter check by implementing `Ash.Policy.FilterCheck`:

```elixir
defmodule MyApp.Checks.VisibleToUserLevel do
  use Ash.Policy.FilterCheck

  # Provide a description (optional as it can be derived from the filter)
  def describe(opts) do
    "records with visibility level at or below actor's level"
  end

  # Return an expression that filters the records
  def filter(actor, _authorizer, _opts) do
    # This filter will only show records with visibility_level
    # less than or equal to the actor's user_level
    expr(visibility_level <= ^actor.user_level)
  end
end

# Usage in policies
policy action_type(:read) do
  authorize_if MyApp.Checks.VisibleToUserLevel
end
```

## Calculations

Calculations allow you to define derived values based on a resource's attributes or related data. Define calculations in the `calculations` block of a resource:

```elixir
calculations do
  # Simple expression calculation
  calculate :full_name, :string, expr(first_name <> " " <> last_name)

  # Expression with conditions
  calculate :status_label, :string, expr(
    cond do
      status == :active -> "Active"
      status == :pending -> "Pending Review"
      true -> "Inactive"
    end
  )

  # Using module calculations for more complex logic
  calculate :risk_score, :integer, {MyApp.Calculations.RiskScore, min: 0, max: 100}
end
```

### Expression Calculations

Expression calculations use Ash expressions and can be pushed down to the data layer when possible:

```elixir
calculations do
  # Simple string concatenation
  calculate :full_name, :string, expr(first_name <> " " <> last_name)

  # Math operations
  calculate :total_with_tax, :decimal, expr(amount * (1 + tax_rate))

  # Date manipulation
  calculate :days_since_created, :integer, expr(
    date_diff(^now(), inserted_at, :day)
  )
end
```

### Module Calculations

For complex calculations, create a module that implements `Ash.Resource.Calculation`:

```elixir
defmodule MyApp.Calculations.FullName do
  use Ash.Resource.Calculation

  # Validate and transform options
  @impl true
  def init(opts) do
    {:ok, Map.put_new(opts, :separator, " ")}
  end

  # Specify what data needs to be loaded
  @impl true
  def load(_query, _opts, _context) do
    [:first_name, :last_name]
  end

  # Implement the calculation logic
  @impl true
  def calculate(records, opts, _context) do
    Enum.map(records, fn record ->
      [record.first_name, record.last_name]
      |> Enum.reject(&is_nil/1)
      |> Enum.join(opts.separator)
    end)
  end
end

# Usage in a resource
calculations do
  calculate :full_name, :string, {MyApp.Calculations.FullName, separator: ", "}
end
```

### Calculations with Arguments

You can define calculations that accept arguments:

```elixir
calculations do
  calculate :full_name, :string, expr(first_name <> ^arg(:separator) <> last_name) do
    argument :separator, :string do
      allow_nil? false
      default " "
      constraints [allow_empty?: true, trim?: false]
    end
  end
end
```

### Using Calculations

Load calculations in queries or on records:

```elixir
# In a query
User
|> Ash.Query.load(:full_name)
|> MyDomain.read!()

# With arguments
User
|> Ash.Query.load(full_name: [separator: ", "])
|> MyDomain.read!()

# On existing records
users = MyDomain.list_users!()
users_with_calcs = Ash.load!(users, :full_name)

# Filter and sort by calculations
User
|> Ash.Query.filter(full_name(separator: " ") == "John Doe")
|> Ash.Query.sort(full_name: {%{separator: " "}, :asc})
|> MyDomain.read!()
```

### Code Interface for Calculations

Define calculation functions on your domain for standalone use:

```elixir
# In your domain
resource User do
  define_calculation :full_name, args: [:first_name, :last_name, {:optional, :separator}]
end

# Then call it directly
MyDomain.full_name("John", "Doe", ", ")  # Returns "John, Doe"
```

## Aggregates

Aggregates allow you to retrieve summary information over groups of related data, like counts, sums, or averages. Define aggregates in the `aggregates` block of a resource:

```elixir
aggregates do
  # Count the number of published posts for a user
  count :published_post_count, :posts do
    filter expr(published == true)
  end

  # Sum the total amount of all orders
  sum :total_sales, :orders, :amount

  # Check if a user has any admin roles
  exists :is_admin, :roles do
    filter expr(name == "admin")
  end
end
```

### Aggregate Types

- **count**: Counts related items meeting criteria
- **sum**: Sums a field across related items
- **exists**: Returns boolean indicating if matching related items exist
- **first**: Gets the first related value matching criteria
- **list**: Lists the related values for a specific field
- **max**: Gets the maximum value of a field
- **min**: Gets the minimum value of a field
- **avg**: Gets the average value of a field

### Using Aggregates

Load aggregates in queries or on records:

```elixir
# In a query
User
|> Ash.Query.load(:published_post_count)
|> MyDomain.read!()

# On existing records
users = MyDomain.list_users!()
users_with_counts = Ash.load!(users, :published_post_count)
```

Filter and sort by aggregates:

```elixir
# Filter users with more than 5 published posts
User
|> Ash.Query.filter(published_post_count > 5)
|> MyDomain.read!()

# Sort users by their post count
User
|> Ash.Query.sort(published_post_count: :desc)
|> MyDomain.read!()
```

### Join Filters

For complex aggregates involving multiple relationships, use join filters:

```elixir
aggregates do
  sum :redeemed_deal_amount, [:redeems, :deal], :amount do
    # Filter on the aggregate as a whole
    filter expr(redeems.redeemed == true)

    # Apply filters to specific relationship steps
    join_filter :redeems, expr(redeemed == true)
    join_filter [:redeems, :deal], expr(active == parent(require_active))
  end
end
```

### Inline Aggregates

Use aggregates inline within expressions:

```elixir
calculate :grade_percentage, :decimal, expr(
  count(answers, query: [filter: expr(correct == true)]) * 100 /
  count(answers)
)
```

## Testing

When testing resources:
- Test your domain actions through the code interface
- Test authorization policies work as expected using `Ash.can?`
- Use `authorize?: false` in tests where authorization is not the focus
- Write generators using `Ash.Generator`
