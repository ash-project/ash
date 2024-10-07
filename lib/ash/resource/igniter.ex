defmodule Ash.Resource.Igniter do
  @moduledoc """
  Codemods for working with Ash.Resource modules

  ## Important Details

  This interrogates *the source code* of a resource, not its ultimate compiled state.
  What this means, is that things like `defines_attribute` will not return `true` if
  the attribute is added by an extension. Only if it appears literally in the source code
  of the resource or one of its `Spark.Dsl.Fragment`s.
  """

  @doc "List all resource modules found in the project"
  def list_resources(igniter) do
    Igniter.Project.Module.find_all_matching_modules(igniter, fn _mod, zipper ->
      zipper
      |> Igniter.Code.Module.move_to_use(resource_mods(igniter))
      |> case do
        {:ok, _} ->
          true

        _ ->
          false
      end
    end)
  end

  @doc "Gets the domain from the given resource module"
  @spec domain(Igniter.t(), Ash.Resource.t()) ::
          {:ok, Igniter.t(), Ash.Domain.t()} | {:error, Igniter.t()}
  def domain(igniter, resource) do
    case Igniter.Project.Module.find_module(igniter, resource) do
      {:ok, {igniter, _source, zipper}} ->
        with {:ok, zipper} <- Igniter.Code.Common.move_to_do_block(zipper),
             {:ok, zipper} <-
               Igniter.Code.Module.move_to_use(zipper, resource_mods(igniter)),
             {:ok, zipper} <-
               Igniter.Code.Function.move_to_nth_argument(zipper, 1),
             {:ok, zipper} <- Igniter.Code.Keyword.get_key(zipper, :domain),
             module when not is_nil(module) <-
               Igniter.Project.Module.to_module_name(zipper.node) do
          {:ok, igniter, module}
        else
          _ ->
            {:error, igniter}
        end

      {:error, igniter} ->
        {:error, igniter}
    end
  end

  def resource_mods(igniter) do
    app_name = Igniter.Project.Application.app_name(igniter)

    [Ash.Resource | List.wrap(Application.get_env(app_name, :base_resources))]
  end

  @doc "Adds the given code block to the block of the resource specified"
  def add_block(igniter, resource, block, chunk) do
    Igniter.Project.Module.find_and_update_module!(igniter, resource, fn zipper ->
      with {:ok, zipper} <-
             Igniter.Code.Function.move_to_function_call_in_current_scope(
               zipper,
               block,
               1
             ),
           {:ok, zipper} <- Igniter.Code.Common.move_to_do_block(zipper) do
        {:ok, Igniter.Code.Common.add_code(zipper, chunk)}
      else
        _ ->
          block_with_chunk = """
          #{block} do
            #{chunk}
          end
          """

          {:ok, Igniter.Code.Common.add_code(zipper, block_with_chunk)}
      end
    end)
  end

  @doc "Adds a bypass to the top of the resource's `policies` block"
  def add_bypass(igniter, resource, condition, body) do
    Igniter.Project.Module.find_and_update_module!(igniter, resource, fn zipper ->
      with {:ok, zipper} <-
             Igniter.Code.Function.move_to_function_call_in_current_scope(
               zipper,
               :policies,
               1
             ),
           {:ok, zipper} <- Igniter.Code.Common.move_to_do_block(zipper) do
        bypass =
          quote do
            bypass unquote(condition) do
              unquote(body)
            end
          end
          |> Sourceror.to_string()
          |> Sourceror.parse_string!()

        {:ok, Igniter.Code.Common.add_code(zipper, bypass, :before)}
      else
        _ ->
          bypass =
            quote do
              policies do
                bypass unquote(condition) do
                  unquote(body)
                end
              end
            end
            |> Sourceror.to_string()
            |> Sourceror.parse_string!()

          {:ok, Igniter.Code.Common.add_code(zipper, bypass)}
      end
    end)
  end

  @doc "Adds a policy to the bottom of the resource's `policies` block"
  def add_policy(igniter, resource, condition, body) do
    Igniter.Project.Module.find_and_update_module!(igniter, resource, fn zipper ->
      with {:ok, zipper} <-
             Igniter.Code.Function.move_to_function_call_in_current_scope(
               zipper,
               :policies,
               1
             ),
           {:ok, zipper} <- Igniter.Code.Common.move_to_do_block(zipper) do
        policy =
          quote do
            policy unquote(condition) do
              unquote(body)
            end
          end
          |> Sourceror.to_string()
          |> Sourceror.parse_string!()

        {:ok, Igniter.Code.Common.add_code(zipper, policy, :after)}
      else
        _ ->
          policy =
            quote do
              policies do
                policy unquote(condition) do
                  unquote(body)
                end
              end
            end
            |> Sourceror.to_string()
            |> Sourceror.parse_string!()

          {:ok, Igniter.Code.Common.add_code(zipper, policy)}
      end
    end)
  end

  @doc "Returns true if the given resource defines an attribute with the provided name"
  @spec defines_attribute(Igniter.t(), Ash.Resource.t(), atom()) :: {Igniter.t(), true | false}
  def defines_attribute(igniter, resource, name) do
    {igniter, defines?} =
      if name in [:inserted_at, :updated_at] do
        has_timestamps_call(igniter, resource)
      else
        {igniter, false}
      end

    if defines? do
      {igniter, true}
    else
      Spark.Igniter.find(igniter, resource, fn _, zipper ->
        with {:ok, zipper} <- enter_section(zipper, :attributes),
             {:ok, _zipper} <-
               move_to_one_of_function_call_in_current_scope(
                 zipper,
                 [
                   :attribute,
                   :uuid_primary_key,
                   :integer_primary_key,
                   :uuid_v7_primary_key,
                   :create_timestamp,
                   :update_timestamp
                 ],
                 [2, 3],
                 &Igniter.Code.Function.argument_equals?(&1, 0, name)
               ) do
          {:ok, true}
        else
          _ ->
            :error
        end
      end)
      |> case do
        {:ok, igniter, _module, _value} ->
          {igniter, true}

        {:error, igniter} ->
          {igniter, false}
      end
    end
  end

  @doc "Returns true if the given resource defines an identity with the provided name"
  @spec defines_identity(Igniter.t(), Ash.Resource.t(), atom()) :: {Igniter.t(), true | false}
  def defines_identity(igniter, resource, name) do
    Spark.Igniter.find(igniter, resource, fn _, zipper ->
      with {:ok, zipper} <- enter_section(zipper, :identities),
           {:ok, _zipper} <-
             move_to_one_of_function_call_in_current_scope(
               zipper,
               [:identity],
               [2, 3],
               &Igniter.Code.Function.argument_equals?(&1, 0, name)
             ) do
        {:ok, true}
      else
        _ ->
          :error
      end
    end)
    |> case do
      {:ok, igniter, _module, _value} ->
        {igniter, true}

      {:error, igniter} ->
        {igniter, false}
    end
  end

  @doc "Returns true if the given resource defines an action with the provided name"
  @spec defines_action(Igniter.t(), Ash.Resource.t(), atom()) :: {Igniter.t(), true | false}
  def defines_action(igniter, resource, name) do
    {igniter, defines?} =
      if name in [:create, :read, :update, :destroy] do
        has_default_action(igniter, resource, name)
      else
        {igniter, false}
      end

    if defines? do
      {igniter, true}
    else
      Spark.Igniter.find(igniter, resource, fn _, zipper ->
        with {:ok, zipper} <- enter_section(zipper, :actions),
             {:ok, _zipper} <-
               move_to_one_of_function_call_in_current_scope(
                 zipper,
                 [
                   :create,
                   :update,
                   :read,
                   :destroy,
                   :action
                 ],
                 [2, 3, 4],
                 &Igniter.Code.Function.argument_equals?(&1, 0, name)
               ) do
          {:ok, true}
        else
          _ ->
            :error
        end
      end)
      |> case do
        {:ok, igniter, _module, _value} ->
          {igniter, true}

        {:error, igniter} ->
          {igniter, false}
      end
    end
  end

  @spec ensure_primary_action(Igniter.t(), Ash.Resource.t(), :create | :read | :update | :destroy) ::
          Igniter.t()
  def ensure_primary_action(igniter, resource, type) do
    case has_default_action(igniter, resource, type) do
      {igniter, true} ->
        igniter

      {igniter, false} ->
        case has_action_with_primary(igniter, resource, type) do
          {igniter, true} -> igniter
          {igniter, false} -> add_default_action(igniter, resource, type)
        end
    end
  end

  @doc "Returns true if the given resource defines a relationship with the provided name"
  @spec defines_relationship(Igniter.t(), Ash.Resource.t(), atom()) :: {Igniter.t(), true | false}
  def defines_relationship(igniter, resource, name) do
    Spark.Igniter.find(igniter, resource, fn _, zipper ->
      with {:ok, zipper} <- enter_section(zipper, :relationships),
           {:ok, _zipper} <-
             move_to_one_of_function_call_in_current_scope(
               zipper,
               [
                 :has_one,
                 :has_many,
                 :belongs_to,
                 :many_to_many
               ],
               [2, 3],
               &Igniter.Code.Function.argument_equals?(&1, 0, name)
             ) do
        {:ok, true}
      else
        _ ->
          :error
      end
    end)
    |> case do
      {:ok, igniter, _module, _value} ->
        {igniter, true}

      {:error, igniter} ->
        {igniter, false}
    end
  end

  @doc "Adds the given code block to the resource's `attributes` block if there is no existing attribute with the given name"
  def add_new_attribute(igniter, resource, name, attribute) do
    {igniter, defines?} = defines_attribute(igniter, resource, name)

    if defines? do
      igniter
    else
      add_attribute(igniter, resource, attribute)
    end
  end

  @doc "Adds the given code block to the resource's `identities` block if there is no existing identity with the given name"
  def add_new_identity(igniter, resource, name, identity) do
    {igniter, defines?} = defines_identity(igniter, resource, name)

    if defines? do
      igniter
    else
      add_identity(igniter, resource, identity)
    end
  end

  @doc "Adds the given code block to the resource's `identities` block"
  def add_identity(igniter, resource, identity) do
    add_block(igniter, resource, :identities, identity)
  end

  @doc "Adds the given code block to the resource's `attributes` block"
  def add_attribute(igniter, resource, attribute) do
    add_block(igniter, resource, :attributes, attribute)
  end

  @doc "Adds an action if it doesn't already exist"
  def add_new_action(igniter, resource, name, action) do
    {igniter, defines?} = defines_action(igniter, resource, name)

    if defines? do
      igniter
    else
      add_action(igniter, resource, action)
    end
  end

  @doc "Adds the given code block to the resource's `actions` block"
  def add_action(igniter, resource, action) do
    add_block(igniter, resource, :actions, action)
  end

  @doc "Adds the given code block to the resource's `relationships` block"
  def add_new_relationship(igniter, resource, name, relationship) do
    {igniter, defines?} = defines_relationship(igniter, resource, name)

    if defines? do
      igniter
    else
      add_block(igniter, resource, :relationships, relationship)
    end
  end

  @doc "Adds the given code block to the resource's `relationships` block"
  def add_relationship(igniter, resource, relationship) do
    add_block(igniter, resource, :relationships, relationship)
  end

  @doc "Adds the given code block to the resource's `resource` block"
  def add_resource_configuration(igniter, resource, resource_configuration) do
    add_block(igniter, resource, :resource, resource_configuration)
  end

  @doc "Ensures that created_at and updated_at timestamps exist on the resource"
  def ensure_timestamps(igniter, resource) do
    {igniter, defines_inserted_at?} = defines_attribute(igniter, resource, :inserted_at)
    {igniter, defines_created_at?} = defines_attribute(igniter, resource, :created_at)
    defines_create_timestamp? = defines_inserted_at? || defines_created_at?
    {igniter, defines_updated_at?} = defines_attribute(igniter, resource, :updated_at)

    igniter
    |> then(fn igniter ->
      if defines_create_timestamp? do
        igniter
      else
        add_create_timestamp_call(igniter, resource)
      end
    end)
    |> then(fn igniter ->
      if defines_updated_at? do
        igniter
      else
        add_update_timestamp_call(igniter, resource)
      end
    end)
  end

  defp enter_section(zipper, name) do
    with {:ok, zipper} <-
           Igniter.Code.Function.move_to_function_call_in_current_scope(
             zipper,
             name,
             1
           ) do
      Igniter.Code.Common.move_to_do_block(zipper)
    end
  end

  defp move_to_one_of_function_call_in_current_scope(zipper, [name], arities, pred) do
    Igniter.Code.Function.move_to_function_call_in_current_scope(
      zipper,
      name,
      arities,
      pred
    )
  end

  defp move_to_one_of_function_call_in_current_scope(zipper, [name | rest], arities, pred) do
    case Igniter.Code.Function.move_to_function_call_in_current_scope(
           zipper,
           name,
           arities,
           pred
         ) do
      {:ok, zipper} -> {:ok, zipper}
      :error -> move_to_one_of_function_call_in_current_scope(zipper, rest, arities, pred)
    end
  end

  defp has_default_action(igniter, resource, type) do
    Spark.Igniter.find(igniter, resource, fn _, zipper ->
      with {:ok, zipper} <- enter_section(zipper, :actions),
           {:ok, _zipper} <-
             Igniter.Code.Function.move_to_function_call_in_current_scope(
               zipper,
               :defaults,
               1,
               fn zipper ->
                 with {:ok, zipper} <- Igniter.Code.Function.move_to_nth_argument(zipper, 0),
                      {:ok, _zipper} <-
                        Igniter.Code.List.move_to_list_item(zipper, fn zipper ->
                          if Igniter.Code.Tuple.tuple?(zipper) do
                            case Igniter.Code.Tuple.tuple_elem(zipper, 0) do
                              {:ok, zipper} ->
                                Igniter.Code.Common.nodes_equal?(zipper, type)

                              _ ->
                                false
                            end
                          else
                            Igniter.Code.Common.nodes_equal?(zipper, type)
                          end
                        end) do
                   true
                 else
                   _ -> false
                 end
               end
             ) do
        {:ok, true}
      else
        _ -> :error
      end
    end)
    |> case do
      {:ok, igniter, _module, _value} ->
        {igniter, true}

      {:error, igniter} ->
        {igniter, false}
    end
  end

  @spec has_action_with_primary(Igniter.t(), Ash.Resource.t(), atom()) ::
          {Igniter.t(), true | false}
  def has_action_with_primary(igniter, resource, type) do
    Spark.Igniter.find(igniter, resource, fn _, zipper ->
      with {:ok, zipper} <- enter_section(zipper, :actions),
           {:ok, _zipper} <-
             Igniter.Code.Function.move_to_function_call_in_current_scope(zipper, type, [1, 2]) do
        case Igniter.Code.Common.move_to_do_block(zipper) do
          {:ok, zipper} ->
            Igniter.Code.Function.move_to_function_call_in_current_scope(
              zipper,
              :primary?,
              1,
              &Igniter.Code.Function.argument_equals?(&1, 0, true)
            )

          :error ->
            with {:ok, zipper} <- Igniter.Code.Function.move_to_nth_argument(zipper, 1),
                 {:ok, zipper} <- Igniter.Code.Keyword.get_key(zipper, :primary?),
                 true <- Igniter.Code.Common.nodes_equal?(zipper, true) do
              {:ok, true}
            end
        end
      end
    end)
    |> case do
      {:ok, igniter, _module, _value} ->
        {igniter, true}

      {:error, igniter} ->
        {igniter, false}
    end
  end

  defp add_default_action(igniter, resource, type) do
    Spark.Igniter.find(igniter, resource, fn _, zipper ->
      with {:ok, zipper} <- enter_section(zipper, :actions),
           {:ok, _zipper} <-
             Igniter.Code.Function.move_to_function_call_in_current_scope(
               zipper,
               :defaults,
               1
             ) do
        {:ok, true}
      else
        _ -> :error
      end
    end)
    |> case do
      {:ok, igniter, module, _value} ->
        Igniter.Project.Module.find_and_update_module!(igniter, module, fn zipper ->
          with {:ok, zipper} <- enter_section(zipper, :actions),
               {:ok, _zipper} <-
                 Igniter.Code.Function.move_to_function_call_in_current_scope(
                   zipper,
                   :defaults,
                   1
                 ),
               {:ok, zipper} <- Igniter.Code.List.prepend_new_to_list(zipper, type) do
            {:ok, zipper}
          else
            _ ->
              {:error,
               "Failed to add a default action of type #{inspect(type)} in #{inspect(module)}"}
          end
        end)

      {:error, igniter} ->
        igniter
    end
  end

  defp add_create_timestamp_call(igniter, resource) do
    add_block(igniter, resource, :attributes, "create_timestamp :created_at")
  end

  defp add_update_timestamp_call(igniter, resource) do
    add_block(igniter, resource, :attributes, "update_timestamp :updated_at")
  end

  defp has_timestamps_call(igniter, resource) do
    Spark.Igniter.find(igniter, resource, fn _, zipper ->
      with {:ok, zipper} <- enter_section(zipper, :attributes),
           {:ok, _zipper} <-
             Igniter.Code.Function.move_to_function_call_in_current_scope(
               zipper,
               :timestamps,
               1
             ) do
        {:ok, true}
      else
        _ -> :error
      end
    end)
    |> case do
      {:ok, igniter, _module, _value} ->
        {igniter, true}

      {:error, igniter} ->
        {igniter, false}
    end
  end
end
