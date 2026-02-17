defmodule ETS.Utils do
  @moduledoc """
  Contains helper macros used by `ETS` modules.
  """

  defmacro __using__(_) do
    quote do
      require Logger
      import ETS.Utils
    end
  end

  def take_opt(opts, key, default) do
    val = Keyword.get(opts, key, default)
    {Keyword.drop(opts, [key]), val}
  end

  defmacro catch_error(do: do_block) do
    {func, arity} = __CALLER__.function
    mod = __CALLER__.module

    quote do
      try do
        unquote(do_block)
      rescue
        e in ArgumentError ->
          Logger.error(
            "Unknown ArgumentError in #{inspect(unquote(mod))}.#{unquote(func)}/#{unquote(arity)}: #{inspect(e)}"
          )

          {:error, :unknown_error}
      end
    end
  end

  defmacro catch_table_not_found(table, do: do_block) do
    quote do
      try do
        unquote(do_block)
      rescue
        e in ArgumentError ->
          case :ets.info(unquote(table)) do
            :undefined -> {:error, :table_not_found}
            _ -> reraise(e, __STACKTRACE__)
          end
      end
    end
  end

  defmacro catch_table_already_exists(table_name, do: do_block) do
    quote do
      try do
        unquote(do_block)
      rescue
        e in ArgumentError ->
          case :ets.whereis(unquote(table_name)) do
            :undefined -> reraise(e, __STACKTRACE__)
            _ -> {:error, :table_already_exists}
          end
      end
    end
  end

  defmacro catch_key_not_found(table, key, do: do_block) do
    quote do
      try do
        unquote(do_block)
      rescue
        e in ArgumentError ->
          case ETS.Base.lookup(unquote(table), unquote(key)) do
            {:ok, []} -> {:error, :key_not_found}
            _ -> reraise(e, __STACKTRACE__)
          end
      end
    end
  end

  defmacro catch_bad_records(records, do: do_block) do
    quote do
      try do
        unquote(do_block)
      rescue
        e in ArgumentError ->
          if Enum.any?(unquote(records), &(!is_tuple(&1))) do
            {:error, :invalid_record}
          else
            reraise(e, __STACKTRACE__)
          end
      end
    end
  end

  defmacro catch_record_too_small(table, record, do: do_block) do
    quote do
      try do
        unquote(do_block)
      rescue
        e in ArgumentError ->
          if :ets.info(unquote(table))[:keypos] > tuple_size(unquote(record)) do
            {:error, :record_too_small}
          else
            reraise(e, __STACKTRACE__)
          end
      end
    end
  end

  defmacro catch_records_too_small(table, records, do: do_block) do
    quote do
      try do
        unquote(do_block)
      rescue
        e in ArgumentError ->
          keypos = :ets.info(unquote(table))[:keypos]

          unquote(records)
          |> Enum.filter(&(keypos > tuple_size(&1)))
          |> case do
            [] -> reraise(e, __STACKTRACE__)
            _ -> {:error, :record_too_small}
          end
      end
    end
  end

  defmacro catch_position_out_of_bounds(table, key, pos, do: do_block) do
    quote do
      try do
        unquote(do_block)
      rescue
        e in ArgumentError ->
          unquote(table)
          |> :ets.lookup(unquote(key))
          |> Enum.any?(&(tuple_size(&1) < unquote(pos)))
          |> if do
            {:error, :position_out_of_bounds}
          else
            reraise(e, __STACKTRACE__)
          end
      end
    end
  end

  defmacro catch_positions_out_of_bounds(table, key, element_spec, do: do_block) do
    quote do
      try do
        unquote(do_block)
      rescue
        e in ArgumentError ->
          [result] = :ets.lookup(unquote(table), unquote(key))
          size = tuple_size(result)
          specs = List.wrap(unquote(element_spec))

          if Enum.any?(specs, fn {pos, _} -> pos < 1 or pos > size end) do
            {:error, :position_out_of_bounds}
          else
            reraise(e, __STACKTRACE__)
          end
      end
    end
  end

  defmacro catch_key_update(table, element_spec, do: do_block) do
    quote do
      try do
        unquote(do_block)
      rescue
        e in ArgumentError ->
          info = :ets.info(unquote(table))
          specs = List.wrap(unquote(element_spec))

          if Enum.any?(specs, fn {pos, _} -> pos == info[:keypos] end) do
            {:error, :cannot_update_key}
          else
            reraise(e, __STACKTRACE__)
          end
      end
    end
  end

  defmacro catch_invalid_select_spec(spec, do: do_block) do
    quote do
      try do
        unquote(do_block)
      rescue
        e in ArgumentError ->
          if ETS.Utils.valid_select_spec?(unquote(spec)) do
            reraise(e, __STACKTRACE__)
          else
            {:error, :invalid_select_spec}
          end
      end
    end
  end

  def valid_select_spec?(spec) when is_list(spec) do
    spec
    |> Enum.all?(fn s ->
      s |> is_tuple() and
        s |> tuple_size() == 3 and
        s |> elem(0) |> is_tuple() and
        s |> elem(1) |> is_list() and
        s |> elem(2) |> is_list()
    end)
  end

  def valid_select_spec?(_not_a_list), do: false

  def continuation_table(:"$end_of_table"), do: {:ok, :"$end_of_table"}

  def continuation_table({table, i1, i2, _match_spec, list, i3})
      when is_integer(i1) and is_integer(i2) and is_list(list) and is_integer(i3) do
    {:ok, table}
  end

  def continuation_table({table, _, _, i1, _match_spec, list, i2, i3})
      when is_integer(i1) and is_list(list) and is_integer(i2) and is_integer(i3) do
    {:ok, table}
  end

  def continuation_table(_), do: {:error, :invalid_continuation}

  def valid_continuation?(continuation), do: match?({:ok, _}, continuation_table(continuation))

  defmacro catch_invalid_continuation(continuation, do: do_block) do
    quote do
      try do
        case ETS.Utils.continuation_table(unquote(continuation)) do
          {:ok, :"$end_of_table"} ->
            unquote(do_block)

          {:error, :invalid_continuation} ->
            {:error, :invalid_continuation}

          {:ok, table} ->
            catch_read_protected table do
              catch_table_not_found table do
                unquote(do_block)
              end
            end
        end
      rescue
        e in ArgumentError ->
          if ETS.Utils.valid_continuation?(unquote(continuation)) do
            {:error, :invalid_continuation}
          else
            reraise(e, __STACKTRACE__)
          end
      end
    end
  end

  defmacro catch_write_protected(table, do: do_block) do
    quote do
      try do
        unquote(do_block)
      rescue
        e in ArgumentError ->
          info = :ets.info(unquote(table))

          if info[:protection] == :public or info[:owner] == self() do
            reraise(e, __STACKTRACE__)
          else
            {:error, :write_protected}
          end
      end
    end
  end

  defmacro catch_read_protected(table, do: do_block) do
    quote do
      try do
        unquote(do_block)
      rescue
        e in ArgumentError ->
          info = :ets.info(unquote(table))

          if info[:protection] in [:public, :protected] or info[:owner] == self() do
            reraise(e, __STACKTRACE__)
          else
            {:error, :read_protected}
          end
      end
    end
  end

  defmacro unwrap_or_raise(expr) do
    {func, arity} = __CALLER__.function
    mod = __CALLER__.module

    quote do
      case unquote(expr) do
        {:ok, value} ->
          value

        {:error, reason} ->
          raise "#{inspect(unquote(mod))}.#{unquote(func)}/#{unquote(arity)} returned {:error, #{inspect(reason)}}"
      end
    end
  end

  defmacro unwrap(expr) do
    quote do
      {:ok, value} = unquote(expr)
      value
    end
  end

  defmacro catch_recipient_already_owns_table(table, pid, do: do_block) do
    quote do
      try do
        unquote(do_block)
      rescue
        e in ArgumentError ->
          info = :ets.info(unquote(table))

          case info[:owner] do
            ^unquote(pid) -> {:error, :recipient_already_owns_table}
            _ -> reraise(e, __STACKTRACE__)
          end
      end
    end
  end

  defmacro catch_recipient_not_alive(pid, do: do_block) do
    quote do
      try do
        unquote(do_block)
      rescue
        e in ArgumentError ->
          case process_alive_safe(unquote(pid)) do
            true -> reraise(e, __STACKTRACE__)
            false -> {:error, :recipient_not_alive}
            error -> error
          end
      end
    end
  end

  def process_alive_safe(pid) when is_pid(pid) do
    Process.alive?(pid)
  rescue
    ArgumentError -> {:error, :recipient_not_local}
  end

  def process_alive_safe(_), do: {:error, :recipient_not_pid}

  defmacro catch_sender_not_table_owner(table, do: do_block) do
    quote do
      try do
        unquote(do_block)
      rescue
        e in ArgumentError ->
          info = :ets.info(unquote(table))
          self = self()

          case info[:owner] do
            ^self -> reraise(e, __STACKTRACE__)
            _ -> {:error, :sender_not_table_owner}
          end
      end
    end
  end
end
