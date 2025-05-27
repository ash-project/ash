# What is Ash?

Ash brings the batteries included experience to Elixir applications in a way that isn't tied to any one particular web framework or API/Frontend technology. It acts as the one universal element that you can take with you no matter what changes about your application or how it interfaces with the outside world.

> Through its declarative extensibility, Ash delivers more than you'd expect: Powerful APIs with filtering/sorting/pagination/calculations/aggregations, pub/sub, authorization, rich introspection, GraphQL... It's what empowers this solo developer to build an ambitious ERP!
>
> — Frank Dugan III, System Specialist, SunnyCor Inc.

Ash is an opinionated, composable set of application building blocks designed for extensibility. It shines when building web apps, APIs and services, but can be used for any kind of Elixir application. It integrates with the best that the
Elixir ecoystem has to offer, and sits directly inside a standard Elixiir codebase alongside the rest of your code.

At its heart, Ash is a framework for modeling your application's domain through **Resources** and their **Actions**. These are the fundamental abstractions that everything else builds upon.

## Why Ash?

If you've ever built a web application, you've probably experienced this: you write the same code over and over. Create a user? Write the database schema, API endpoints, validation logic, authorization rules, and tests. Add a new field? Update the database, API, frontend, and documentation separately. Want GraphQL instead of REST? Rewrite everything.

**The problem**: Every piece of functionality gets implemented multiple times across different layers of your application. This creates:
- **Repetitive work** - The same business logic scattered across multiple files
- **Inconsistencies** - Different parts of your app handle the same data differently
- **Maintenance burden** - Change one thing, update five different places
- **Knowledge silos** - Each developer builds things their own way

**Ash's solution**: Model your application's **behavior** first, derive everything else automatically. But this isn't just "slapping an API on a database" - Ash resources center around **actions** that represent rich business operations. Instead of exposing raw data tables, you define meaningful operations like `:publish_post`, `:approve_order`, or `:calculate_shipping_cost` that encapsulate your business logic, validation, and authorization rules.

Why are actions "introspectable"? Because when your code can examine what an action does at runtime, other tools can automatically build around it. Define a "publish post" action with its validation rules and authorization policies, and extensions can automatically generate appropriate REST endpoints, GraphQL mutations, admin interfaces, and API documentation - all respecting your business rules.

This isn't just about convenience - it's about **modeling your domain properly**. When everything derives from well-designed business operations rather than raw database tables, your entire application becomes more maintainable and aligned with how your business actually works.

## Built for Flexibility

Ash is built from the collective experience of working with inflexible abstractions that eventually paint you into a corner. That's why Ash is designed with **multi-tiered configurability** and **escape hatches all the way down**.

**You're never locked out of custom behavior**. Need to customize how an action works? Use preparations, changes, and validations. Need to override how data is fetched? Implement a manual action. Need to completely bypass Ash for a specific operation? Drop down to Ecto or raw SQL. Need to extend Ash itself? Use the same extension toolkit that powers AshPostgres and AshGraphql.

This isn't accidental - it's core to Ash's design. We provide powerful defaults that work for 80% of cases, extensive configuration options for the next 15%, and escape hatches for the remaining 5%. Your Ash application is just an Elixir application, so when you need to do something completely custom, Ash won't get in your way.

The framework acts as a **spinal cord** for your application: providing structure and coordination while allowing complete customization at every level.

> I'd consider Phoenix to be like Flask. Ash fills the gap that brings Phoenix up to feature parity with a batteries included framework like Django.
> Ash Admin (Django admin), Ash Resource & Domain (Django models & ORM), AshJsonApi (Django REST Framework), Ash Authentication (Django Allauth), Ash Phoenix (Django Forms), Ash Policies (Django Permissions)
>
> But you aren't required to use Phoenix with an Ash project. Ash will happily work as a standalone CLI, terminal app or some other Elixir web framework that comes out tomorrow.
>
> Scott Woodall - Principal Software Engineer, Microsoft

## Essential Context

<!-- tabs-open -->

### Elixir Developers

**Ash builds on the best of the Elixir ecosystem** rather than replacing it. Ash leverages Ecto for database operations, Phoenix for web interfaces, Oban for background jobs, Absinthe for GraphQL, etc. Ash unifies all of this tooling and significantly simplifies and enhances they way you use them together.

**Compile-time guarantees** come from Ash's declarative nature - many errors that would be runtime failures in other frameworks become compile-time checks in Ash.

**The ecosystem advantage**: Instead of learning entirely new patterns, Ash enhances familiar Elixir concepts. Your existing knowledge of pattern matching, process supervision, and OTP principles all apply - Ash just provides structure and automation on top, acting as a spinal cord for your application.

### Non-Elixir Developers

**Elixir** is a functional programming language built on the Erlang VM, known for fault-tolerance and concurrency. It's used by WhatsApp, Discord, and Pinterest for systems that need high availability. If you know Ruby or Python, think of Elixir as optimized specifically for distributed, concurrent systems.

**Functional vs Object-Oriented**: Unlike OOP frameworks, Elixir (and Ash) focus on transforming data through functions rather than encapsulating data in objects. This leads to more predictable, testable code.

**Immutability** means data doesn't change in-place - instead, you create new versions. This eliminates many common bugs around shared state and makes concurrent operations safer.

**Actor Model Concurrency**: Instead of threads and locks, Elixir uses lightweight processes (actors) that communicate via messages. This makes Ash naturally suited for high-concurrency applications.

**Modules** (`defmodule`) are Elixir's equivalent to classes/namespaces - they group related functions together. Unlike OOP classes, they don't hold state.

### New Programmers

**Programming languages** like Elixir are tools for writing instructions that computers can follow. Elixir is specifically designed for building web applications that can handle lots of users at once.

**Modules** (the `defmodule` blocks you'll see) are containers that group related code together. Think of them like chapters in a book - each chapter covers a specific topic.

### Business Leaders

**Ash reduces software development costs** by eliminating repetitive code. When your development team defines a business process once, Ash automatically generates the database structure, API endpoints, and user interfaces - work that normally requires separate specialists.

**Faster time-to-market** because features that typically take weeks can be built in days. Adding a new feature like "customer reviews" or "order tracking" requires defining the business rules once, rather than building separate systems for web, mobile, and internal tools.

**Lower maintenance costs** because changes to business rules automatically update all related systems. When you change how orders work, your database, APIs, and documentation stay synchronized without manual updates.

**Reduced technical risk** through built-in security, data validation, and error handling. Your team spends time building your competitive advantages instead of solving the same infrastructure problems every software company faces.

**Developer productivity** increases because the framework handles the "plumbing" while your team focuses on what makes your business unique. This means you need fewer developers to build the same functionality.

**Eliminates technical debt** through enforced consistency. When every part of your application follows the same patterns, there's no accumulation of "quick fixes" and inconsistent approaches that slow down future development and create maintenance headaches.

**Faster developer onboarding** because new team members learn one set of patterns that apply everywhere. Instead of each developer building things differently, Ash provides a shared vocabulary and approach that new hires can quickly understand and contribute to. Ash is still niche, so developers may not know it right out of the gate, but if you think of Ash as a replacement for your internal framework, which are universally poorly documented and hard to train on, you can see the benefit of Ash being an open source, well documented project with a strong community.

<!-- tabs-close -->

## Resources and Actions: The Core Abstractions

The foremost abstraction in Ash is **Actions** - the things you can do in your domain like `:create_user`, `:publish_post`, `:approve_order`, or `:calculate_shipping`. These actions are organized into **Resources** that group related behaviors around domain concepts like `User`, `Post`, `Order`, or `Invoice`. Using resources, you can easily model actions, alongside the state that they operate on, or just actions in isolation.

These actions are **introspectable** (your code can examine them at runtime) and **fully typed**. This means the rest of your application (and **extensions** - add-on packages that enhance Ash) can automatically understand and build functionality around them. When you define a `create` action that accepts a `:title` string and `:content` text, extensions like AshGraphql can automatically generate GraphQL mutations, AshJsonApi can create REST endpoints, and AshPostgres can handle database persistence - all without additional configuration.

This **declarative approach** means your resources become the single source of truth for your entire application. Database schemas, API endpoints, authorization rules, state machines, background jobs, and more all stem directly from your resource definitions.

## Beyond Simple CRUD

Ash is not a web framework, like Phoenix or Rails. It is a framework for building your application layer, independent of how it is exposed or consumed. It is not an **alternative** to frameworks like Phoenix, rather a **complement** to them.

> Ash Framework enabled us to build a robust platform for delivering financial services using bitcoin. Ash proved itself to our team by handling innovative use cases with ease and it continues to evolve ahead of our growing list of needs.
>
> — Yousef Janajri, CTO & Co-Founder, Coinbits

The intent behind Ash is _not_ to have you building simple CRUD-style applications, although we do provide conveniences for these cases. The real power comes from defining rich, domain-specific actions with meaningful names like `:publish_post`, `:approve_order`, or `:calculate_shipping`. These actions encapsulate your business logic and can be composed, validated, authorized, and extended in powerful ways.

> #### Model your domain, derive the rest {: .info}
>
> Ash derives significant portions of your application directly from your resources, with little to no effort required. This allows you to focus on what matters most: your business logic.
> We leverage the best of the Elixir ecosystem under the hood, providing a single unified tool-chain for our users.

## Why should I use it?

The fundamental idea behind Ash is that when the various components of your system can have consistent expectations of how the other components around them work, you can ultimately do a significant amount more, with less. For example, once you've defined your resources, it takes only a few additional lines of code to have your database structure generated, and a full featured API built around it.

Ash has many use cases, with varying degrees of complexity. Ash helps you on day 1 of your project, removing boilerplate and allowing you to focus on the essential complexity of your application. It also helps you on year 5, lending consistency, code reuse, and maintainability.

> #### Ash is a force multiplier {: .tip}
>
> **Adding a GraphQL API**: Traditionally requires setting up Absinthe schemas, resolvers, and type definitions across multiple files. With Ash: add `extensions: [AshGraphql.Resource]` and `type :post` - you're done.
>
> **Database + API + Validation**: Normally requires separate Ecto schemas, Phoenix controllers, changesets, and API serializers. With Ash: define one resource with actions, get all of them automatically synchronized.
>
> **Preventing API/Database drift**: Traditional approaches require manual synchronization between your API documentation, validation rules, and database constraints. Ash eliminates this entire category of bugs because everything derives from the same source.

## Community

Ash has a vibrant community of developers who contribute to the project, provide support, and share knowledge. Join us on [Discord](https://discord.gg/w3AXeARR2p), [ElixirForum](https://elixirforum.com/ash) and [GitHub](https://github.com/ash-project/ash) to contribute, ask questions, and stay updated on the latest developments.

Our community is one of the best features of Ash and **you should use it**. Lots of folks using Ash in production, with a shared mission of making better software.

> I'm constantly blown away with the quality of work and support the Ash community has put into this project. It's gotten to the point that I can't imagine starting a new Elixir project that doesn't use Ash.
>
> — Brett Kolodny, Full stack engineer, MEW

## A Complete Example: From Simple to Sophisticated

To illustrate how Ash grows with your application, let's start with a simple blog post resource and see how extensions enhance it:

> #### Don't worry about understanding all the code {: .info}
>
> The following examples are meant to give you a sense of how Ash works and grows with your application. You're not expected to understand every detail of the syntax - focus on the concepts and how it changes over time.

### 1. Basic Resource

```elixir
# lib/my_blog/blog.ex
defmodule MyBlog.Blog do
  use Ash.Domain

  resources do
    resource MyBlog.Blog.Post do
      # Define idiomatic context functions on-demand
      # This is opt-in, no auto-magic function definitions
      # based on action names, you're in control
      define :analyze_text, args: [:text]
    end
  end
end

# lib/my_blog/blog/post.ex
defmodule MyBlog.Blog.Post do
  use Ash.Resource

  actions do
    # Start with pure behavior - a simple action that processes text
    action :analyze_text, :map do
      argument :text, :string, allow_nil?: false

      run fn input, _context ->
        text = input.arguments.text
        words = String.split(text)

        analysis = %{
          word_count: length(words),
          character_count: String.length(text),
          estimated_reading_time: div(length(words), 200) + 1
        }

        {:ok, analysis}
      end
    end
  end
end
```

Notice how we have not defined any "state" here. A common misconception is that the purpose of Ash is to abstract state. Ash provides tons of useful features around state, but those are features *on top of* the core concept, which centers around typed actions.

This demonstrates that Ash isn't about "slapping an API on a database" - it's about modeling meaningful business operations first. The `analyze_text` action represents a domain operation, not just data manipulation.

```elixir
{:ok, analysis} = MyBlog.Blog.analyze_text("This is some sample blog content to analyze.")
# => {:ok, %{word_count: 9, character_count: 49, estimated_reading_time: 1}}
```

**Why not just write a regular function?** You could write `def analyze_text(text)` and get the same result. But here's what the Ash action gives you that a function doesn't:

- **Type safety** - Arguments are automatically validated (try passing a number instead of string)
- **Introspection** - Other tools can discover this action exists and what it does
- **Extensibility** - You can add authorization, logging, or other behaviors later without changing callers
- **API generation** - Extensions can automatically expose this as a REST endpoint or GraphQL query
- **Consistent interface** - All actions work the same way, making your codebase predictable

The action is **typed** (it knows it takes a string and returns a map) and **introspectable** (your application can examine it at runtime). This means extensions can automatically understand and build on top of it.

> ### You could stop here {: .info}
>
> You could stop at this step and still derive significant value from Ash. You wouldn't be
> using it wrong. You can build your own custom state system under the hood, use Phoenix contexts,
> call directly into Ecto, totally up to you.

### 2. Adding State and Persistence with AshPostgres

Now let's add state to support persistent storage, while keeping our existing behavior:

```elixir
# lib/my_blog/blog.ex
defmodule MyBlog.Blog do
  use Ash.Domain

  resources do
    resource MyBlog.Blog.Post do
      define :analyze_text, args: [:text]
      define :create_post, action: :create, args: [:title, :content]
    end
  end
end

# lib/my_blog/blog/post.ex
defmodule MyBlog.Blog.Post do
  use Ash.Resource,
    domain: MyBlog.Blog,
    data_layer: AshPostgres.DataLayer # data_layer tells Ash where to store data

  postgres do
    table "posts"
    repo MyBlog.Repo
  end

  attributes do
    uuid_primary_key :id
    attribute :title, :string, allow_nil?: false, public?: true
    attribute :content, :string, public?: true
    attribute :status, :atom, constraints: [one_of: [:draft, :published]], default: :draft, public?: true

    create_timestamp :created_at, public?: true
    update_timestamp :updated_at, public?: true
  end

  actions do
    action :analyze_text, :map do
      argument :text, :string, allow_nil?: false

      run fn input, _context ->
        text = input.arguments.text
        words = String.split(text)

        analysis = %{
          word_count: length(words),
          character_count: String.length(text),
          estimated_reading_time: div(length(words), 200) + 1
        }

        {:ok, analysis}
      end
    end

    defaults [:read, :destroy, create: [:title, :content], update: [:title, :content, :status]]
  end
end
```

Now your resource combines behavior and state. The original `analyze_text` action still works, plus you can create and persist posts:

```elixir
# Behavior still works exactly the same
{:ok, analysis} = MyBlog.Blog.analyze_text("Some text to analyze")

# Now we can also persist state
{:ok, post} = MyBlog.Blog.create_post("My First Post", "This is some content")
```

### 3. Adding GraphQL API with AshGraphql

This is just one example of an API extension. We also have [ash_json_api](https://hexdocs.pm/ash_json_api) with more on the way.
Ash also comes with all the tools you need to build *your own* API extension.

The types & implementations

```elixir
# Add to your domain
defmodule MyBlog.Blog do
  use Ash.Domain,
    extensions: [AshGraphql.Domain]

  graphql do
    queries do
      action MyApp.Blog.Post, :analyze_text, :analyze_text
    end

    mutations do
      create MyApp.Blog.Post, :create_post, :create
    end
  end

  # ... resources
end

# Add to your resource
defmodule MyBlog.Blog.Post do
  use Ash.Resource,
    domain: MyBlog.Blog,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshGraphql.Resource]

  graphql do
    # you just tell us the name of the type
    # we can take care of the rest
    # there is deep configuration with plenty of escape hatches
    # for when you need something bespoke
    type :post
  end

  # ... rest of resource definition
end
```

Now you have a full GraphQL API with queries, mutations, and custom actions automatically generated from your resource definition.
That is actually all of the code you need to do it.

> The ease of defining our domain model and configuring Ash to generate a powerful GraphQL API has been a game-changer. What used to be complex and time-consuming has become simplicity itself.
>
> — Alan Heywood, CTO, HereTask

### 4. Adding Encryption with AshCloak

```elixir
defmodule MyBlog.Blog.Post do
  use Ash.Resource,
    domain: MyBlog.Blog,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshGraphql.Resource, AshCloak.Resource]

  cloak do
    vault MyBlog.Vault
    attributes [:content]  # Encrypt the content attribute
  end

  # ... rest of resource definition
end
```

Your post content is now automatically encrypted at rest and decrypted when read, with no changes to your existing API or business logic.

### 5. Adding State Management with AshStateMachine

```elixir
defmodule MyBlog.Blog.Post do
  use Ash.Resource,
    domain: MyBlog.Blog,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshGraphql.Resource, AshCloak.Resource, AshStateMachine]

  state_machine do
    initial_states [:draft]
    default_initial_state :draft

    transitions do
      transition :publish, from: :draft, to: :published
      transition :unpublish, from: :published, to: :draft
    end
  end

  actions do
    # ... same actions as before
    update :publish do
      change transition_state(:published)
      # ... additional custom logic on publish
    end

    update :unpublish do
      change transition_state(:draft)
      # ... additional custom logic on publish
    end
  end

  # ... rest of resource definition
end
```

Now your posts have a proper state machine with transition actions, state validation, and automatic GraphQL mutations for state changes.

## The Ash Advantage

This example demonstrates Ash's core philosophy: **Model your domain, derive the rest**. Notice how:

- The resource definition remained largely unchanged as we added each extension
- Each extension automatically understood and enhanced the existing actions
- Complex functionality (encryption, state machines, APIs) required minimal configuration
- Everything remains introspectable and type-safe
- Your business logic stays focused on the domain, not infrastructure concerns
- These extensions are built with the same suite of tools we provide *to the user* for extending Ash.
  Those using Ash will often end up crafting their own extensions, making the framework truly their own.

Extensions work together seamlessly because they all operate on the same well-defined resource and action abstractions. In the example above, we went from a single action to a full-featured system with database persistence, GraphQL API, encryption, and state management - by adding configuration, not writing code.

**The productivity gain is measurable**: What normally requires separate database migration files, API endpoint definitions, validation logic, GraphQL schema files, and state management code becomes a single, cohesive resource definition that stays automatically synchronized.
