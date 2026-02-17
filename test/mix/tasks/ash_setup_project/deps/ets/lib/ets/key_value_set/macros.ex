defmodule ETS.KeyValueSet.Macros do
  @moduledoc false

  use ETS.Utils

  defmacro __using__(_) do
    quote do
      require Logger
      import ETS.KeyValueSet.Macros
    end
  end

  defmacro delegate_to_set(fun_name, arity, opts \\ [], args)

  defmacro delegate_to_set(fun_name, 1 = arity, opts, do: short_desc) do
    desc1 = "#{short_desc}. See `ETS.Set.#{fun_name}/#{arity}`."
    desc2 = "Same as `#{fun_name}/#{arity}` but unwraps or raises on error."
    fun_name_bang = String.to_atom("#{fun_name}!")

    ret = Keyword.get(opts, :ret, quote(do: any()))

    unwrap_and_raise_or_not =
      if Keyword.get(opts, :can_raise, true) do
        quote(do: :unwrap_or_raise)
      else
        quote(do: :unwrap)
      end

    quote do
      alias ETS.KeyValueSet
      alias ETS.Set

      @doc unquote(desc1)
      @spec unquote(fun_name)(KeyValueSet.t()) :: {:ok, unquote(ret)} | {:error, any()}
      def unquote(fun_name)(%KeyValueSet{set: set}), do: Set.unquote(fun_name)(set)

      @doc unquote(desc2)
      @spec unquote(fun_name_bang)(KeyValueSet.t()) :: unquote(ret)
      def unquote(fun_name_bang)(%KeyValueSet{} = key_value_set),
        do: unquote(unwrap_and_raise_or_not)(KeyValueSet.unquote(fun_name)(key_value_set))
    end
  end

  defmacro delegate_to_set(fun_name, 2 = arity, opts, do: short_desc) do
    desc1 = "#{short_desc}. See `ETS.Set.#{fun_name}/#{arity}`."
    desc2 = "Same as `#{fun_name}/#{arity}` but unwraps or raises on error."
    fun_name_bang = String.to_atom("#{fun_name}!")

    ret = Keyword.get(opts, :ret, quote(do: any()))
    second_param_type = Keyword.get(opts, :second_param_type, quote(do: any()))

    unwrap_and_raise_or_not =
      if Keyword.get(opts, :can_raise, true) do
        quote(do: :unwrap_or_raise)
      else
        quote(do: :unwrap)
      end

    quote do
      alias ETS.KeyValueSet
      alias ETS.Set

      @doc unquote(desc1)
      @spec unquote(fun_name)(KeyValueSet.t(), unquote(second_param_type)) ::
              {:ok, unquote(ret)} | {:error, any()}
      def unquote(fun_name)(%KeyValueSet{set: set}, key), do: Set.unquote(fun_name)(set, key)

      @doc unquote(desc2)
      @spec unquote(fun_name_bang)(KeyValueSet.t(), unquote(second_param_type)) :: unquote(ret)
      def unquote(fun_name_bang)(%KeyValueSet{} = key_value_set, key),
        do: unquote(unwrap_and_raise_or_not)(KeyValueSet.unquote(fun_name)(key_value_set, key))
    end
  end
end
