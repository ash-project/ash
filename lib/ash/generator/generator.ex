# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Generator do
  @moduledoc """
  Tools for generating input to Ash resource actions and for generating seed data.

  ## Using Ash.Generator

  To define generators for your tests, `use Ash.Generator`, and define
  functions that use `changeset_generator/3` and/or `seed_generator/2`.

  ```elixir
  defmodule YourApp.Generator do
    use Ash.Generator

    # using `seed_generator`, bypasses the action and saves directly to the data layer
    def blog_post(opts \\\\ []) do
      seed_generator(
        %MyApp.Blog.Post{
          name: sequence(:title, &"My Blog Post \#{&1}")
          text: StreamData.repeatedly(fn -> Faker.Lorem.paragraph() end)
        },
        overrides: opts
      )
    end

    # using `changeset_generator`, calls the action when passed to `generate`
    def blog_post_comment(opts \\\\ []) do
      blog_post_id = opts[:blog_post_id] || once(:default_blog_post_id, fn -> generate(blog_post()).id end)

      changeset_generator(
        MyApp.Blog.Comment,
        :create,
        defaults: [
          blog_post_id: blog_post_id
        ],
        overrides: opts
      )
    end
  end
  ```

  Then, in your tests, you can `import YourApp.Generator`, and use `generate/1` and `generate_many/1` to generate data.
  For example:

  ```elixir
  import YourApp.Generator

  test "`comment_count` on blog_post shows the count of comments" do
    blog_post = generate(blog_post())
    assert Ash.load!(blog_post, :comment_count).comment_count == 0

    generate_many(blog_post_comment(blog_post_id: blog_post.id), 10)

    assert Ash.load!(blog_post, :comment_count).comment_count == 10
  end
  ```

  ## About Generators

  These generators are backed by `StreamData`, and are ready for use with property testing via `ExUnitProperties`

  Many functions in this module support "overrides", which allow passing down either constant values
  or your own `StreamData` generators.

  For example:

  ```elixir
  # All generated posts will have text as `"text"`. Equivalent to providing `StreamData.constant("text")`.
  Ash.Generator.seed_input(Post, %{text: "text"})
  ```
  """

  @dialyzer {:nowarn_function,
             seed_input: 2,
             seed_input: 1,
             seed!: 1,
             seed!: 2,
             seed_many!: 2,
             seed_many!: 3,
             action_input: 2,
             action_input: 3,
             changeset: 2,
             changeset: 3,
             changeset: 4,
             do_mixed_map: 1,
             query: 2,
             query: 3,
             query: 4,
             seed_generator: 1,
             seed_generator: 2,
             changeset_generator: 2,
             changeset_generator: 3,
             generate_attributes: 5,
             mixed_map: 2,
             many_changesets: 3,
             many_changesets: 4,
             many_changesets: 5,
             many_queries: 3,
             many_queries: 4,
             many_queries: 5,
             do_changeset_or_query: 5}

  @typedoc """
  A map or keyword of data generators or constant values to use in place of defaults.

  Many functions in `Ash.Generator` support `overrides`, allowing to customize the default
  generated values.
  """
  @type overrides ::
          %{term() => stream_data() | term()} | Keyword.t(stream_data() | term())

  @typedoc """
  An instance of `StreamData`, gotten from one of the functions in that module.
  """
  @type stream_data :: Enumerable.t()

  defmacro __using__(_opts) do
    quote do
      import Ash.Generator, except: [generate: 1, generate_many: 2]

      defdelegate generate(generator), to: Ash.Generator
      defdelegate generate_many(generator, count), to: Ash.Generator
    end
  end

  @doc """
  A generator of changesets which call their specific actions when passed to `generate/1` or `generate_many/2`.

  See `seed_generator/2` for the equivalent construct for cases when you want to seed directly to the data layer as opposed to calling resource
  actions.

  ## Examples

  ```elixir
  iex> changeset_generator(MyApp.Blog.Post, :create, defaults: [title: sequence(:blog_post_title, &"My Blog Post \#{&1}")]) |> generate()
  %Ash.Changeset{...}
  ```

  ## Usage in tests

  This can be used to define generators in tests. A useful pattern is defining a function like so:

  ```elixir
  def blog_post(opts \\ []) do
    changeset_generator(
      MyApp.Blog.Post,
      :create,
      defaults: [
        name: sequence(:blog_post_title, &"My Blog Post \#{&1}")
        text: StreamData.repeatedly(fn -> Faker.Lorem.paragraph() end)
      ],
      overrides: opts
    )
  end
  ```

  When you only allow child resource to be created through a managed relationship, e.g. an update action on a parent resource,
  this pattern could be expanded, yielding a resource with a new child resource:

  ```elixir
  def post_for(author, opts \\ []) do
    changeset_generator(
      author,
      :new_post,
      uses: [
        post_input:
          action_input(
            MyApp.Blog.Post,
            :create,
            title: sequence(:title, &"Post \#{&1}")
          )
      ],
      defaults: fn inputs ->
        [posts: [inputs.post_input]]
      end,
      overrides: opts
    )
  end
  ```

  See the `Ash.Generator` moduledocs for more information.

  ## Options

  * `:defaults` - A keyword list of values or generators, used as inputs. Can also be a function
    when using the `:uses` option.
  * `:overrides` - A keyword list or map of `t:overrides()`
  * `:actor` - Passed through to the changeset
  * `:tenant` - Passed through to the changeset
  * `:scope` - Passed through to the changeset
  * `:uses` - A map of generators that are passed into your `defaults`. `defaults` must be a
    function. This is useful when multiple things in your `defaults` need to use the same generated
    value.
  * `:authorize?` - Passed through to the changeset
  * `:context` - Passed through to the changeset
  * `:after_action` - A one argument function that takes the result and returns
    a new result to run after the record is created.
  * `:private_arguments` - A map of private arguments, whose values can also be generators. Can also
    be a function when using the `:uses` option.

  ## The `uses` option

  ```elixir
  def blog_post(opts \\ []) do
    changeset_generator(
      MyApp.Blog.Post,
      :create,
      uses: [
        author: author() # A function using `changeset_generator` just like this one.
      ],
      defaults: fn %{author: author} ->
        author = generate(author)

        [
          name: sequence(:blog_post_title, &"My Blog Post \#{&1}")
          author_name: author.name,
          text: StreamData.repeatedly(fn -> Faker.Lorem.paragraph() end)
        ]
      end
      overrides: opts
    )
  end
  ```

  """
  def changeset_generator(resource, action, opts \\ []) do
    generator =
      if opts[:uses] do
        if not (is_nil(opts[:defaults]) || is_function(opts[:defaults])) do
          raise ArgumentError,
                "The `uses` option can only be provided if the `defaults` option is a function"
        end

        if not (is_nil(opts[:private_arguments]) || is_function(opts[:private_arguments])) do
          raise ArgumentError,
                "The `uses` option can only be provided if the `private_arguments` option is a function"
        end

        opts[:uses]
        |> to_generators()
        |> StreamData.fixed_map()
        |> StreamData.bind(fn uses ->
          generators =
            if opts[:defaults] do
              opts[:defaults].(uses)
              |> Map.new()
            else
              %{}
            end
            |> Map.merge(Map.new(opts[:overrides] || %{}))

          private_arguments =
            if opts[:private_arguments] do
              opts[:private_arguments].(uses)
              |> to_generators()
              |> StreamData.fixed_map()
            else
              StreamData.fixed_map(%{})
            end

          changeset_opts =
            StreamData.fixed_map(
              Map.put(
                to_generators(
                  Keyword.take(opts, [
                    :actor,
                    :tenant,
                    :scope,
                    :authorize?,
                    :context,
                    :upsert?,
                    :upsert_identity
                  ])
                ),
                :private_arguments,
                private_arguments
              )
            )

          StreamData.fixed_map(%{
            changeset_opts: changeset_opts,
            input: action_input(resource, action, generators)
          })
        end)
      else
        if is_function(opts[:defaults]) do
          raise ArgumentError,
                "The `uses` option must be provided if the `defaults` option is a function"
        end

        generators =
          opts[:defaults]
          |> Kernel.||([])
          |> Map.new()
          |> Map.merge(Map.new(opts[:overrides] || %{}))

        input = action_input(resource, action, generators)

        private_arguments =
          if opts[:private_arguments] do
            StreamData.fixed_map(to_generators(opts[:private_arguments]))
          else
            StreamData.fixed_map(%{})
          end

        changeset_opts =
          StreamData.fixed_map(
            Map.put(
              to_generators(
                Keyword.take(opts, [
                  :actor,
                  :tenant,
                  :scope,
                  :authorize?,
                  :context,
                  :upsert?,
                  :upsert_identity
                ])
              ),
              :private_arguments,
              private_arguments
            )
          )

        StreamData.fixed_map(%{
          changeset_opts: changeset_opts,
          input: input
        })
      end

    generator
    |> StreamData.map(fn %{input: input, changeset_opts: changeset_opts} ->
      Ash.Changeset.for_action(
        resource,
        action,
        input,
        Map.to_list(changeset_opts)
      )
      |> then(fn changeset ->
        if opts[:after_action] do
          Ash.Changeset.after_action(changeset, fn _changeset, record ->
            {:ok, opts[:after_action].(record)}
          end)
          |> Ash.Changeset.set_context(%{private: %{generator_after_action: opts[:after_action]}})
        else
          changeset
        end
      end)
    end)
  end

  @doc """
  A generator of seedable records, to be passed to `generate/1` or `generate_many/1`

  See `changeset_generator/3` for the equivalent construct for cases when you want to call resource
  actions as opposed to seed directly to the data layer.

  When a struct is given, only exactly the given values/generators will be used. If you
  pass a tuple, i.e `{Resource, %{field: :value}}`, all values not provided will be generated
  automatically.

  ## Examples

  ```elixir
  iex> seed_generator(%MyApp.Blog.Post{name: sequence(:blog_post_title, &"My Blog Post \#{&1}")}) |> generate()
  %Tunez.Music.Artist{name: "Artist 1"}

  iex> seed_generator({MyApp.Blog.Post, %{}}) |> generate()
  %Tunez.Music.Artist{name: "A random name"}
  ```

  ## Usage in tests

  This can be used to define seed generators in tests. A useful pattern is defining a function like so:

  ```elixir
  def blog_post(opts \\ []) do
    seed_generator(
      %MyApp.Blog.Post{
        name: sequence(:blog_post_title, &"My Blog Post \#{&1}")
        text: StreamData.repeatedly(fn -> Faker.Lorem.paragraph() end)
      },
      overrides: opts
    )
  end
  ```

  See the `Ash.Generator` moduledocs for more information.

  ## Options

  * `:overrides` - A keyword list or map of `t:overrides()`
  * `:actor` - Passed through to the changeset
  * `:tenant` - Passed through to the changeset
  * `:scope` - Passed through to the changeset
  * `:uses` - A map of generators that are passed into the first argument, if it is a function.
  * `:authorize?` - Passed through to the changeset
  * `:context` - Passed through to the changeset
  * `:after_action` - A one argument function that takes the result and returns
    a new result to run after the record is created.
  """
  @spec seed_generator(
          Ash.Resource.record()
          | {Ash.Resource.t(), map()}
          | (map -> Ash.Resource.record() | {Ash.Resource.t(), %{}}),
          opts :: Keyword.t()
        ) :: stream_data()
  def seed_generator(record, opts \\ []) do
    if opts[:uses] do
      if !is_function(record) do
        raise ArgumentError, "The `uses` option can only be provided if `record` is a function"
      end

      opts[:uses]
      |> to_generators()
      |> StreamData.fixed_map()
      |> StreamData.bind(fn uses ->
        resource_or_record =
          record.(uses)

        resource =
          case resource_or_record do
            {resource, _record} -> resource
            %resource{} -> resource
          end

        resource_or_record
        |> case do
          %_{} = record ->
            record
            |> Map.take(Enum.to_list(Ash.Resource.Info.attribute_names(resource)))
            |> Enum.reduce(%{}, fn {key, value}, acc ->
              attribute = Ash.Resource.Info.attribute(resource, key)

              if !attribute.writable? && attribute.generated? do
                acc
              else
                Map.put(acc, key, value)
              end
            end)
            |> Map.merge(Map.new(opts[:overrides] || %{}))
            |> to_generators()
            |> Map.put(:__will_be_struct__, resource)
            |> StreamData.fixed_map()

          {resource, attributes} ->
            resource
            |> Ash.Resource.Info.attributes()
            |> Enum.reject(&(!&1.writable? && &1.generated?))
            |> generate_attributes(
              to_generators(
                Map.put(
                  Map.merge(attributes, Map.new(opts[:overrides] || %{})),
                  :__will_be_struct__,
                  resource
                )
              ),
              false,
              :create,
              []
            )
        end
      end)
      |> StreamData.map(fn keys ->
        Ash.Resource.set_metadata(
          struct(keys.__will_be_struct__, keys),
          opts
          |> Keyword.take([:tenant])
          |> Enum.into(%{private: %{generator_after_action: opts[:after_action]}})
        )
      end)
    else
      if is_function(record) do
        raise ArgumentError, "The `uses` option must be provided if `record` is a function"
      end

      resource =
        case record do
          {resource, _record} -> resource
          %resource{} -> resource
        end

      record
      |> case do
        %_{} = record ->
          record
          |> Map.take(Enum.to_list(Ash.Resource.Info.attribute_names(resource)))
          |> Enum.reduce(%{}, fn {key, value}, acc ->
            attribute = Ash.Resource.Info.attribute(resource, key)

            if !attribute.writable? && attribute.generated? do
              acc
            else
              Map.put(acc, key, value)
            end
          end)
          |> Map.merge(Map.new(opts[:overrides] || %{}))
          |> to_generators()
          |> StreamData.fixed_map()

        {_resource, attributes} ->
          resource
          |> Ash.Resource.Info.attributes()
          |> Enum.reject(&(!&1.writable? && &1.generated?))
          |> generate_attributes(
            to_generators(Map.merge(attributes, Map.new(opts[:overrides] || %{}))),
            false,
            :create,
            []
          )
      end
      |> StreamData.map(fn keys ->
        Ash.Resource.set_metadata(
          struct(resource, keys),
          opts
          |> Keyword.take([:tenant])
          |> Enum.into(%{private: %{generator_after_action: opts[:after_action]}})
        )
      end)
    end
  end

  @doc """
  Generate globally unique values.

  This is useful for generating values that are unique within a given test or processes that it spawns, such as email addresses,
  or for generating values that are unique across a single resource, such as identifiers. The values will be unique
  for anything using the same sequence name, **within the same test**.

  > ### Not Globally Unique {: .warning}
  > The lifecycle of this generator is tied to the process that initially starts it. In general,
  > that will be the test. In the rare case where you are running async processes that need to share a sequence
  > that is not created in the test process, you can initialize a sequence in the test using `initialize_sequence/1`.
  >
  > If you need a globally unique value, use a value like `System.unique_integer([:positive])` in your values instead.
  >
  > For example:
  >
  > ```elixir
  > StreamData.repeatedly(fn -> "email\#{System.unique_integer([:positive])}@example.com" end)
  > ```

  Example:

      Ash.Generator.sequence(:unique_email, fn i -> "user\#\{i\}@example.com" end) |> Enum.take(3)
      iex> ["user0@example.com", "user1@example.com", "user2@example.com"]

  ## Using a different sequencer

  By default we use an incrementing integer starting at 0. However, if you want to use something else, you can provide
  your own sequencer. The initial value will be `nil`, which you can use to detect that you are the start of the sequence.

  Example:

      Ash.Generator.sequence(:unique_email, fn i -> "user\#\{i\}@example.com" end, fn num -> (num || 1) - 1 end) |> Enum.take(3)
      iex> ["user0@example.com", "user-1@example.com", "user-2@example.com"]
  """
  @spec sequence(pid | atom, (iterator | nil -> value), (iterator | nil -> iterator)) ::
          StreamData.t(value)
        when iterator: term, value: term
  def sequence(identifier, generator, sequencer \\ fn i -> (i || -1) + 1 end) do
    pid =
      if is_pid(identifier) do
        identifier
      else
        initialize_sequence(identifier)
      end

    StreamData.repeatedly(fn ->
      Agent.get_and_update(pid, fn state ->
        next_in_sequence = sequencer.(state)
        value = generator.(next_in_sequence)
        {value, next_in_sequence}
      end)
    end)
  end

  @doc """
  Run the provided function or enumerable (i.e generator) only once.

  This is useful for ensuring that some piece of data is generated a single time during a test.

  The lifecycle of this generator is tied to the process that initially starts it. In general,
  that will be the test. In the rare case where you are running async processes that need to share a sequence
  that is not created in the test process, you can initialize a sequence in the test using `initialize_once/1`.

  Example:

      iex> Ash.Generator.once(:user, fn ->
             register_user(...)
           end) |> Enum.at(0)
      %User{id: 1} # created the user

      iex> Ash.Generator.once(:user, fn ->
             register_user(...)
           end) |> Enum.at(0)
      %User{id: 1} # reused the last user
  """
  @spec once(pid | term, (-> value) | Enumerable.t(value)) ::
          StreamData.t(value)
        when value: term
  def once(identifier, generator) do
    pid =
      if is_pid(identifier) do
        identifier
      else
        initialize_once(identifier)
      end

    StreamData.repeatedly(fn ->
      generate_once(pid, generator)
    end)
  end

  defp generate_once(pid, generator) do
    Agent.get_and_update(pid, fn state ->
      case state do
        :locked ->
          {:locked, :locked}

        :none ->
          {{:generate, generator}, :locked}

        {:some, value} ->
          {{:value, value}, {:some, value}}
      end
    end)
    |> case do
      :locked ->
        generate_once(pid, generator)

      {:generate, generator} ->
        new =
          case generator do
            generator when is_function(generator) ->
              generator.()

            value ->
              Enum.at(value, 0)
          end

        Agent.update(pid, fn _state ->
          {:some, new}
        end)

        new

      {:value, value} ->
        value
    end
  end

  @doc """
  Starts and links an agent for a `once/2`, or returns the existing agent pid if it already exists.

  See `once/2` for more.
  """
  # sobelow_skip ["DOS.BinToAtom"]
  @spec initialize_once(term()) :: pid
  def initialize_once(identifier) do
    case find_in_self_or_ancestor({:ash_once, identifier}) do
      nil ->
        {:ok, pid} = Agent.start_link(fn -> :none end)
        Process.put({:ash_once, identifier}, pid)
        pid

      pid ->
        pid
    end
  end

  @doc """
  Stops the agent for a `once/2`.

  See `once/2` for more.
  """
  def stop_once(identifier) do
    Agent.stop(identifier)
    :ok
  end

  @doc """
  Starts and links an agent for a sequence, or returns the existing agent pid if it already exists.

  See `sequence/3` for more.
  """
  @spec initialize_sequence(atom) :: pid
  # sobelow_skip ["DOS.BinToAtom"]
  def initialize_sequence(identifier) do
    case find_in_self_or_ancestor({:ash_sequence, identifier}) do
      nil ->
        {:ok, pid} = Agent.start_link(fn -> nil end)
        Process.put({:ash_sequence, identifier}, pid)
        pid

      pid ->
        pid
    end
  end

  defp find_in_self_or_ancestor(key) do
    Enum.find_value([self()] ++ (Process.get(:"$ancestors", []) || []), fn pid ->
      pid
      |> Process.info(:dictionary)
      |> elem(1)
      |> Enum.find_value(fn {found_key, value} ->
        if found_key == key do
          value
        end
      end)
    end)
  end

  @doc """
  Stops the agent for a sequence.

  See `sequence/3` for more.
  """
  def stop_sequence(identifier) do
    Agent.stop(identifier)
    :ok
  end

  @doc """
  Gets input using `seed_input/2` and passes it to `Ash.Seed.seed!/2`, returning the result
  """
  def seed!(resource, generators \\ %{}) do
    input =
      seed_input(resource, generators)
      |> Enum.at(0)

    Ash.Seed.seed!(resource, input)
  end

  @doc """
  Generates an input `n` times, and passes them all to seed, returning the list of seeded items.
  """
  def seed_many!(resource, n, generators \\ %{}) do
    seed_input(resource, generators)
    |> Stream.take(n)
    |> Enum.map(&Ash.Seed.seed!(resource, &1))
  end

  @doc """
  Takes one value from a changeset or seed generator and calls `Ash.create!` or `Ash.update!` on it.

  Passes through resource structs without doing anything.
  Creates a changeset if given
  """
  @spec generate(
          stream_data()
          | Ash.Changeset.t()
          | Ash.Resource.record()
        ) ::
          Ash.Resource.record()
  def generate(%Ash.Changeset{action_type: :create} = changeset) do
    Ash.create!(changeset)
  end

  def generate(%Ash.Changeset{action_type: :update} = changeset) do
    Ash.update!(changeset)
  end

  def generate(changeset_generator) do
    if is_struct(changeset_generator) and
         Ash.Resource.Info.resource?(changeset_generator.__struct__) do
      if changeset_generator.__meta__.state == :built do
        result = Ash.Seed.seed!(changeset_generator)

        case changeset_generator.__metadata__[:private][:generator_after_action] do
          nil ->
            result

          after_action ->
            after_action.(result)
        end
      else
        changeset_generator
      end
    else
      changeset_generator |> Enum.at(0) |> generate()
    end
  end

  @doc """
  Takes `count` values from a changeset or seed generator and passes their inputs into `Ash.bulk_create!` or `Ash.Seed.seed!` respectively.
  """
  def generate_many(changeset_generator, count) do
    changeset_generator
    |> Stream.take(count)
    |> Stream.chunk_every(500)
    |> Enum.flat_map(fn
      [%Ash.Changeset{} = first | _rest] = batch ->
        after_action = first.context[:private][:generator_after_action]

        opts = [
          return_records?: true,
          return_errors?: true,
          notify?: true,
          sorted?: true,
          max_concurrency: 0,
          stop_on_error?: true,
          actor: first.context[:private][:actor],
          authorize?: first.context[:private][:authorize?],
          tenant: first.tenant,
          tracer: first.context[:private][:tracer]
        ]

        opts =
          if after_action do
            Keyword.put(opts, :after_action, fn _changeset, record ->
              {:ok, after_action.(record)}
            end)
          else
            opts
          end

        result =
          Ash.bulk_create!(Enum.map(batch, & &1.params), first.resource, first.action.name, opts)

        if result.status != :success do
          raise Ash.Error.to_error_class(result.errors)
        end

        result.records || []

      batch ->
        Enum.map(batch, &generate/1)
    end)
  end

  @doc """
  Generate input meant to be passed into a resource action.

  Arguments that are passed to a `manage_relationship` are not generated by default, and you will
  have to generate them yourself by passing your own generators/values down. See the module documentation for more.
  """
  @spec action_input(
          Ash.Resource.t() | Ash.Resource.record(),
          action_name :: atom,
          generators :: overrides()
        ) :: map()
  def action_input(resource_or_record, action_name, generators \\ %{}) do
    resource =
      case resource_or_record do
        %resource{} -> resource
        resource -> resource
      end

    action = Ash.Resource.Info.action(resource, action_name)

    if !action do
      raise ArgumentError,
            "Invalid action #{inspect(resource)}.#{action_name}"
    end

    arguments =
      Enum.filter(action.arguments, fn argument ->
        argument.public? && !find_manage_change(argument, action)
      end)

    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.filter(&(&1.name in Map.get(action, :accept, [])))
    |> set_allow_nil(action)
    |> Enum.concat(arguments)
    |> generate_attributes(generators, false, action.type, Enum.map(action.arguments, & &1.name))
  end

  @doc """
  Creates the input for the provided action with `action_input/3`, and creates a changeset for that action with that input.

  See `action_input/3` and the module documentation for more.
  """
  @spec changeset(
          Ash.Resource.t(),
          action :: atom(),
          overrides(),
          changeset_options :: Keyword.t()
        ) :: Ash.Changeset.t()
  def changeset(resource_or_record, action, generators \\ %{}, changeset_options \\ []) do
    resource =
      case resource_or_record do
        %resource{} -> resource
        resource -> resource
      end

    input =
      action_input(resource_or_record, action, generators)
      |> Enum.at(0)

    do_changeset_or_query(resource, resource_or_record, action, input, changeset_options)
  end

  @doc """
  Generate `count` changesets and return them as a list.
  """
  @spec many_changesets(
          Ash.Resource.t(),
          action :: atom(),
          count :: pos_integer(),
          overrides(),
          changeset_options :: Keyword.t()
        ) :: list(Ash.Changeset.t())
  def many_changesets(
        resource_or_record,
        action,
        count,
        generators \\ %{},
        changeset_options \\ []
      ) do
    resource =
      case resource_or_record do
        %resource{} -> resource
        resource -> resource
      end

    action_input(resource_or_record, action, generators)
    |> Enum.take(count)
    |> Enum.map(fn input ->
      do_changeset_or_query(resource, resource_or_record, action, input, changeset_options)
    end)
  end

  @doc """
  Creates the input for the provided action with `action_input/3`, and returns a query for that action with that input.

  See `action_input/3` and the module documentation for more.
  """
  @spec query(
          Ash.Resource.t(),
          action :: atom(),
          overrides(),
          query_options :: Keyword.t()
        ) :: Ash.Query.t()
  def query(resource, action, generators \\ %{}, query_options \\ []) do
    changeset(resource, action, generators, query_options)
  end

  @doc """
  Generate `count` queries and return them as a list.
  """
  @spec many_queries(
          Ash.Resource.t(),
          action :: atom(),
          count :: pos_integer(),
          overrides(),
          changeset_options :: Keyword.t()
        ) :: list(Ash.Query.t())
  def many_queries(resource, action, count, generators \\ %{}, changeset_options \\ []) do
    many_changesets(resource, action, count, generators, changeset_options)
  end

  @doc """
  Generate input meant to be passed into `Ash.Seed.seed!/2`.

  A map of custom `StreamData` generators can be provided to add to or overwrite the generated input,
  for example: `Ash.Generator.seed_input(Post, %{text: StreamData.constant("Post")})`
  """
  @spec seed_input(Ash.Resource.t(), map()) :: StreamData.t(map())
  def seed_input(resource, generators \\ %{}) do
    relationships = Ash.Resource.Info.relationships(resource)

    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.reject(fn attribute ->
      Enum.any?(relationships, &(&1.source_attribute == attribute.name))
    end)
    |> generate_attributes(generators, true, :create, Enum.map(relationships, & &1.name))
  end

  @doc """
  Gets the next value for a given sequence identifier.

  See `sequence/3` for more.

  This is equivalent to `identifier |> Ash.Generator.sequence(fun, sequencer) |> Enum.at(0)`
  """
  def next_in_sequence(identifier, fun, sequencer \\ fn i -> (i || -1) + 1 end) do
    identifier
    |> sequence(fun, sequencer)
    |> Enum.at(0)
  end

  @doc """
  Creates a generator of maps where all keys are required except the list provided

  ## Example

  ```elixir
  iex> mixed_map(%{a: StreamData.constant(1), b: StreamData.constant(2)}, [:b]) |> Enum.take(2)
  [%{a: 1}, %{a: 1, b: 2}]
  ```
  """
  @spec mixed_map(map(), list(term())) :: stream_data()
  def mixed_map(map, []) do
    map = to_generators(map)
    StreamData.fixed_map(map)
  end

  def mixed_map(map, keys) do
    map = to_generators(map)
    {optional, required} = Map.split(map, keys)
    do_mixed_map({StreamData.fixed_map(required), StreamData.optional_map(optional)})
  end

  defp do_changeset_or_query(resource, resource_or_record, action, input, changeset_options) do
    case Ash.Resource.Info.action(resource, action).type do
      :read ->
        Ash.Query.for_read(resource, action, input, changeset_options)

      :create ->
        Ash.Changeset.for_create(resource, action, input, changeset_options)

      :update ->
        Ash.Changeset.for_update(resource_or_record, action, input, changeset_options)

      :destroy ->
        Ash.Changeset.for_destroy(resource_or_record, action, input, changeset_options)
    end
  end

  defp generate_attributes(
         attributes,
         generators,
         keep_nil?,
         action_type,
         extra_keys_to_keep
       ) do
    generators = Map.new(generators)

    attributes
    |> Enum.reduce({%{}, %{}}, fn attribute, {required, optional} ->
      default =
        cond do
          action_type == :create ->
            attribute.default

          action_type in [:update, :destroy] and is_struct(attribute, Ash.Resource.Attribute) ->
            attribute.update_default

          true ->
            nil
        end

      # only create a value for attributes that didn't get a dedicated generator
      if attribute.name in Map.keys(generators) do
        {required, optional}
      else
        if attribute.allow_nil? || !is_nil(default) do
          options = [attribute_generator(attribute, attribute.allow_nil?)]

          options =
            if attribute.allow_nil? do
              if keep_nil? do
                [StreamData.constant(:__keep_nil__) | options]
              else
                [StreamData.constant(nil) | options]
              end
            else
              options
            end

          options =
            if attribute.allow_nil? && is_nil(default) do
              [StreamData.constant(nil) | options]
            else
              options
            end

          {required,
           Map.put(
             optional,
             attribute.name,
             StreamData.one_of(options)
           )}
        else
          {Map.put(
             required,
             attribute.name,
             attribute_generator(attribute, attribute.allow_nil?)
           ), optional}
        end
      end
    end)
    |> then(fn {required, optional} ->
      generators =
        generators
        |> to_generators()
        |> Map.take(Enum.map(attributes, & &1.name) ++ extra_keys_to_keep)

      {Map.merge(required, generators), Map.drop(optional, Map.keys(generators))}
    end)
    |> do_mixed_map()
  end

  defp attribute_generator(attribute, false) do
    attribute.type
    |> Ash.Type.generator(attribute.constraints)
    |> StreamData.filter(fn item ->
      with {:ok, value} <- Ash.Type.cast_input(attribute.type, item, attribute.constraints),
           {:ok, nil} <-
             Ash.Type.apply_constraints(attribute.type, value, attribute.constraints) do
        false
      else
        _ ->
          true
      end
    end)
  end

  defp attribute_generator(attribute, true) do
    Ash.Type.generator(attribute.type, attribute.constraints)
  end

  defp do_mixed_map({required, optional}) do
    {StreamData.fixed_map(required), StreamData.optional_map(optional)}
    |> StreamData.map(fn {required, optional} ->
      Map.merge(required, optional)
    end)
  end

  defp to_generators(generators) do
    Map.new(generators, fn {key, value} ->
      case value do
        %StreamData{} ->
          {key, value}

        value ->
          {key, StreamData.constant(value)}
      end
    end)
  end

  defp set_allow_nil(
         attributes,
         %{
           allow_nil_input: allow_nil_input,
           require_attributes: require_attributes
         }
       ) do
    Enum.map(attributes, fn attribute ->
      cond do
        attribute.name in allow_nil_input ->
          %{attribute | allow_nil?: true}

        attribute.name in require_attributes ->
          %{attribute | allow_nil?: false}

        true ->
          attribute
      end
    end)
  end

  defp set_allow_nil(attributes, _), do: attributes

  defp find_manage_change(argument, action) do
    Enum.find_value(Map.get(action, :changes, []), fn
      %{change: {Ash.Resource.Change.ManageRelationship, opts}} ->
        if opts[:argument] == argument.name do
          opts
        end

      _ ->
        nil
    end)
  end
end
