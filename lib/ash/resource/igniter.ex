defmodule Ash.Resource.Igniter do
  @moduledoc "Codemods for working with Ash.Resource modules"

  @doc "List all resource modules found in the project"
  def list_resources(igniter) do
    Igniter.Code.Module.find_all_matching_modules(igniter, fn _mod, zipper ->
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
    case Igniter.Code.Module.find_module(igniter, resource) do
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
    Igniter.Code.Module.find_and_update_module!(igniter, resource, fn zipper ->
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
    Igniter.Code.Module.find_and_update_module!(igniter, resource, fn zipper ->
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
                policy unquote(condition) do
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
    Igniter.Code.Module.find_and_update_module!(igniter, resource, fn zipper ->
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

  @doc "Adds the given code block to the resource's `attributes` block"
  def add_attribute(igniter, resource, attribute) do
    add_block(igniter, resource, :attributes, attribute)
  end

  @doc "Adds the given code block to the resource's `actions` block"
  def add_action(igniter, resource, action) do
    add_block(igniter, resource, :actions, action)
  end

  @doc "Adds the given code block to the resource's `relationships` block"
  def add_relationship(igniter, resource, relationship) do
    add_block(igniter, resource, :relationships, relationship)
  end

  @doc "Adds the given code block to the resource's `resource` block"
  def add_resource_configuration(igniter, resource, resource_configuration) do
    add_block(igniter, resource, :resource, resource_configuration)
  end
end
