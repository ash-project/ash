defmodule Ash.Generator do
  @moduledoc """
  Tools for generating input to Ash resource actions, as well as for seeds.

  These tools are young, and various factors are not taken into account. For example,
  validations and identities are not automatically considered.

  If you want to use this with stream data testing, you will likely want to get familiar with `StreamData`.

  Many functions in this module support overrides, which allow passing down either constant values
  or your own generators.

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
             generate_attributes: 4,
             mixed_map: 2,
             many_changesets: 3,
             many_changesets: 4,
             many_changesets: 5,
             many_queries: 3,
             many_queries: 4,
             many_queries: 5,
             do_changeset_or_query: 5}

  @doc "Creates a generator map where the keys are required except the list provided"
  def mixed_map(map, keys) do
    map = to_generators(map)
    {optional, required} = Map.split(map, keys)
    do_mixed_map({StreamData.fixed_map(required), StreamData.optional_map(optional)})
  end

  @doc """
  Generate input meant to be passed into `Ash.Seed.seed!/2`.

  A map of custom `StreamData` generators can be provided to add to or overwrite the generated input,
  for example: `Ash.Generator.for_seed(Post, %{text: StreamData.constant("Post")})`
  """
  def seed_input(resource, generators \\ %{}) do
    relationships = Ash.Resource.Info.relationships(resource)

    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.reject(fn attribute ->
      Enum.any?(relationships, &(&1.source_attribute == attribute.name))
    end)
    |> generate_attributes(generators, true, :create)
  end

  @doc """
  Generate globally unique values.

  This is useful for generating values that are unique across all resources, such as email addresses,
  or for generating values that are unique across a single resource, such as identifiers. The values will be unique
  for anything using the same sequence name.

  The name of the identifier will be used as the name of the agent process, so use a unique name not in use anywhere else.

  The lifecycle of this generator is tied to the process that initially starts it. In general,
  that will be the test. In the rare case where you are running async processes that need to share a sequence
  that is not created in the test process, you can initialize a sequence in the test using `initialize_sequence/1`.

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
  Starts and links an agent for a sequence, or returns the existing agent pid if it already exists.

  See `sequence/3` for more.
  """
  @spec initialize_sequence(atom) :: pid
  def initialize_sequence(identifier) do
    case Agent.start_link(fn -> nil end, name: identifier) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
    end
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

    Ash.Seed.seed!(input)
  end

  @doc """
  Generates an input `n` times, and passes them all to seed, returning the list of seeded items.
  """
  def seed_many!(resource, n, generators \\ %{}) do
    seed_input(resource, generators)
    |> Enum.take(n)
    |> Enum.map(&Ash.Seed.seed!(resource, &1))
  end

  @doc """
  Generate input meant to be passed into a resource action.

  Currently input for arguments that are passed to a `manage_relationship` are excluded, and you will
  have to generate them yourself by passing your own generators/values down. See the module documentation for more.
  """
  def action_input(resource_or_record, action, generators \\ %{}) do
    resource =
      case resource_or_record do
        %resource{} -> resource
        resource -> resource
      end

    action = Ash.Resource.Info.action(resource, action)

    arguments = Enum.reject(action.arguments, &find_manage_change(&1, action))

    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.filter(&(&1.name in action.accept))
    |> set_allow_nil(action)
    |> Enum.concat(arguments)
    |> generate_attributes(generators, false, action.type)
  end

  @doc """
  Creates the input for the provided action with `action_input/3`, and creates a changeset for that action with that input.

  See `action_input/3` and the module documentation for more.
  """
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
  Generate n changesets and return them as a list.
  """
  def many_changesets(resource_or_record, action, n, generators \\ %{}, changeset_options \\ []) do
    resource =
      case resource_or_record do
        %resource{} -> resource
        resource -> resource
      end

    action_input(resource_or_record, action, generators)
    |> Enum.take(n)
    |> Enum.map(fn input ->
      do_changeset_or_query(resource, resource_or_record, action, input, changeset_options)
    end)
  end

  @doc """
  Creates the input for the provided action with `action_input/3`, and creates a query for that action with that input.

  See `action_input/3` and the module documentation for more.
  """
  def query(resource, action, generators \\ %{}, changeset_options \\ []) do
    changeset(resource, action, generators, changeset_options)
  end

  @doc """
  Generate n queries and return them as a list.
  """
  def many_queries(resource, action, n, generators \\ %{}, changeset_options \\ []) do
    many_changesets(resource, action, n, generators, changeset_options)
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

  defp generate_attributes(attributes, generators, keep_nil?, action_type) do
    attributes
    |> Enum.reduce({%{}, %{}}, fn attribute, {required, optional} ->
      default =
        cond do
          action_type == :create ->
            attribute.default

          action_type in [:update, :destroy] ->
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
      {Map.merge(required, to_generators(generators)), Map.drop(optional, Map.keys(generators))}
    end)
    |> do_mixed_map()
  end

  defp attribute_generator(attribute, false) do
    attribute.type
    |> Ash.Type.generator(attribute.constraints)
    |> StreamData.filter(fn item ->
      case Ash.Type.apply_constraints(attribute.type, item, attribute.constraints) do
        {:ok, nil} -> false
        _ -> true
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
