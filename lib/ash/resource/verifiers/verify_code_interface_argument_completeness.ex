defmodule Ash.Resource.Verifiers.VerifyCodeInterfaceArgumentCompleteness do
  @moduledoc """
  Verifies that code interfaces declare all required action arguments in their args list.
  
  This prevents runtime Protocol.UndefinedError when users call interface functions
  with positional arguments that should map to action arguments.
  """
  use Spark.Dsl.Verifier

  @impl true
  def verify(dsl) do
    dsl
    |> Ash.Resource.Info.interfaces()
    |> Enum.each(&verify_interface!(dsl, &1))
    
    :ok
  end

  defp verify_interface!(dsl, interface) do
    action = Ash.Resource.Info.action(dsl, interface.action || interface.name)
    module = Spark.Dsl.Verifier.get_persisted(dsl, :module)
    
    if action do
      required_arguments = get_required_arguments(action)
      interface_args = normalize_interface_args(interface.args || [])
      
      # Account for custom inputs that transform to action arguments
      custom_input_targets = get_custom_input_targets(interface)
      
      # Account for explicitly excluded inputs
      excluded_inputs = interface.exclude_inputs || []
      
      # Arguments that are satisfied by custom inputs or excluded don't need to be in args
      satisfied_args = Enum.uniq(custom_input_targets ++ excluded_inputs)
      
      missing_args = (required_arguments -- interface_args) -- satisfied_args
      
      if missing_args != [] do
        raise_missing_args_error(module, interface, action, missing_args)
      end
    end
  end

  defp get_required_arguments(action) do
    action.arguments
    |> Enum.filter(fn arg ->
      # Required if: not allow_nil? and no default value
      !arg.allow_nil? && is_nil(arg.default)
    end)
    |> Enum.map(& &1.name)
  end

  defp normalize_interface_args(args) do
    Enum.map(args, fn
      {:optional, arg} -> arg
      arg -> arg
    end)
  end

  defp get_custom_input_targets(interface) do
    interface.custom_inputs
    |> Enum.filter(& &1.transform)
    |> Enum.map(& &1.transform.to)
  end

  defp raise_missing_args_error(module, interface, action, missing_args) do
    action_name = action.name
    interface_name = interface.name
    
    suggestion = """
    code_interface do
      define :#{interface_name}, args: #{inspect(missing_args)}
    end
    """
    
    raise Spark.Error.DslError,
      module: module,
      message: """
      Code interface `#{interface_name}` is missing required arguments for action `#{action_name}`.
      
      The action `#{action_name}` has required arguments: #{inspect(missing_args)}
      But the code interface doesn't declare them in the `args:` option.
      
      This will cause runtime errors when calling the interface function.
      
      Fix by updating your code interface:
      
      #{suggestion}
      
      Alternatively, if these arguments should be optional, add default values
      or set `allow_nil? true` in the action arguments.
      """,
      path: [:code_interface, interface_name]
  end
end