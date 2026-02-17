defmodule ETS.Base do
  @moduledoc """
  Base implementation for table modules (e.g. `ETS.Set` and `ETS.Bag`). Should not be used directly.

  """
  use ETS.Utils

  @protection_types [:public, :protected, :private]
  @type option ::
          {:name, atom()}
          | {:protection, :private | :protected | :public}
          | {:heir, :none | {pid(), any()}}
          | {:keypos, non_neg_integer()}
          | {:write_concurrency, boolean()}
          | {:read_concurrency, boolean()}
          | {:compressed, boolean()}
  @type options :: [option]
  @type table_types :: :bag | :duplicate_bag | :ordered_set | :set
  @table_types [:bag, :duplicate_bag, :ordered_set, :set]

  @doc false
  @spec new_table(table_types(), keyword()) ::
          {:ok, {ETS.table_reference(), keyword()}} | {:error, any()}
  def new_table(type, opts) when type in @table_types and is_list(opts) do
    {opts, name} = take_opt(opts, :name, nil)

    if is_atom(name) do
      starting_opts =
        if is_nil(name) do
          [type]
        else
          [:named_table, type]
        end

      case parse_opts(starting_opts, opts) do
        {:ok, parsed_opts} ->
          catch_table_already_exists name do
            info =
              name
              |> :ets.new(parsed_opts)
              |> :ets.info()

            ref = info[:id]
            {:ok, {ref, info}}
          end

        {:error, reason} ->
          {:error, reason}
      end
    else
      {:error, {:invalid_option, {:name, name}}}
    end
  end

  @spec parse_opts(list(), options) :: {:ok, list()} | {:error, {:invalid_option, any()}}
  defp parse_opts(acc, [{:protection, protection} | tl]) when protection in @protection_types,
    do: parse_opts([protection | acc], tl)

  defp parse_opts(acc, [{:heir, {pid, heir_data}} | tl]) when is_pid(pid),
    do: parse_opts([{:heir, pid, heir_data} | acc], tl)

  defp parse_opts(acc, [{:heir, :none} | tl]), do: parse_opts([{:heir, :none} | acc], tl)

  defp parse_opts(acc, [{:keypos, keypos} | tl]) when is_integer(keypos) and keypos >= 0,
    do: parse_opts([{:keypos, keypos} | acc], tl)

  defp parse_opts(acc, [{:write_concurrency, wc} | tl]) when is_boolean(wc),
    do: parse_opts([{:write_concurrency, wc} | acc], tl)

  defp parse_opts(acc, [{:read_concurrency, rc} | tl]) when is_boolean(rc),
    do: parse_opts([{:read_concurrency, rc} | acc], tl)

  defp parse_opts(acc, [{:compressed, true} | tl]), do: parse_opts([:compressed | acc], tl)
  defp parse_opts(acc, [{:compressed, false} | tl]), do: parse_opts(acc, tl)

  defp parse_opts(acc, []), do: {:ok, acc}

  defp parse_opts(_, [bad_val | _]),
    do: {:error, {:invalid_option, bad_val}}

  @doc false
  @spec info(ETS.table_identifier()) :: {:ok, keyword()} | {:error, :table_not_found}
  def info(table) do
    catch_error do
      case :ets.info(table) do
        :undefined -> {:error, :table_not_found}
        x -> {:ok, x}
      end
    end
  end

  @doc false
  @spec insert(ETS.table_identifier(), tuple(), any()) :: {:ok, any()} | {:error, any()}
  def insert(table, record, return) do
    catch_error do
      catch_write_protected table do
        catch_record_too_small table, record do
          catch_table_not_found table do
            :ets.insert(table, record)
            {:ok, return}
          end
        end
      end
    end
  end

  @doc false
  @spec insert_new(ETS.table_identifier(), tuple(), any()) :: {:ok, any()} | {:error, any()}
  def insert_new(table, record, return) do
    catch_error do
      catch_write_protected table do
        catch_record_too_small table, record do
          catch_table_not_found table do
            :ets.insert_new(table, record)
            {:ok, return}
          end
        end
      end
    end
  end

  @doc false
  @spec insert_multi(ETS.table_identifier(), list(tuple()), any()) ::
          {:ok, any()} | {:error, any()}
  def insert_multi(table, records, return) do
    catch_error do
      catch_write_protected table do
        catch_records_too_small table, records do
          catch_bad_records records do
            catch_table_not_found table do
              :ets.insert(table, records)
              {:ok, return}
            end
          end
        end
      end
    end
  end

  @doc false
  @spec insert_multi_new(ETS.table_identifier(), list(tuple), any()) ::
          {:ok, any()} | {:error, any()}
  def insert_multi_new(table, records, return) do
    catch_error do
      catch_write_protected table do
        catch_records_too_small table, records do
          catch_bad_records records do
            catch_table_not_found table do
              :ets.insert_new(table, records)
              {:ok, return}
            end
          end
        end
      end
    end
  end

  @doc false
  @spec to_list(ETS.table_identifier()) :: {:ok, [tuple()]} | {:error, any()}
  def to_list(table) do
    catch_error do
      catch_table_not_found table do
        {:ok, :ets.tab2list(table)}
      end
    end
  end

  @doc false
  @spec lookup(ETS.table_identifier(), any()) :: {:ok, [tuple()]} | {:error, any()}
  def lookup(table, key) do
    catch_error do
      catch_read_protected table do
        catch_table_not_found table do
          vals = :ets.lookup(table, key)
          {:ok, vals}
        end
      end
    end
  end

  @doc false
  @spec lookup_element(ETS.table_identifier(), any(), non_neg_integer()) ::
          {:ok, any()} | {:error, any()}
  def lookup_element(table, key, pos) do
    catch_error do
      catch_position_out_of_bounds table, key, pos do
        catch_key_not_found table, key do
          catch_read_protected table do
            catch_table_not_found table do
              vals = :ets.lookup_element(table, key, pos)
              {:ok, vals}
            end
          end
        end
      end
    end
  end

  @doc false
  @spec match(ETS.table_identifier(), ETS.match_pattern()) :: {:ok, [tuple()]} | {:error, any()}
  def match(table, pattern) do
    catch_error do
      catch_read_protected table do
        catch_table_not_found table do
          matches = :ets.match(table, pattern)
          {:ok, matches}
        end
      end
    end
  end

  @doc false
  @spec match(ETS.table_identifier(), ETS.match_pattern(), non_neg_integer()) ::
          {:ok, {[tuple()], any()}} | {:error, any()}
  def match(table, pattern, limit) do
    catch_error do
      catch_read_protected table do
        catch_table_not_found table do
          case :ets.match(table, pattern, limit) do
            {x, :"$end_of_table"} -> {:ok, {x, :end_of_table}}
            {records, continuation} -> {:ok, {records, continuation}}
            :"$end_of_table" -> {:ok, {[], :end_of_table}}
          end
        end
      end
    end
  end

  @doc false
  @spec match(any()) :: {:ok, {[tuple()], any() | :end_of_table}} | {:error, any()}
  def match(continuation) do
    catch_error do
      try do
        case :ets.match(continuation) do
          {x, :"$end_of_table"} -> {:ok, {x, :end_of_table}}
          {records, continuation} -> {:ok, {records, continuation}}
          :"$end_of_table" -> {:ok, {[], :end_of_table}}
        end
      rescue
        ArgumentError ->
          {:error, :invalid_continuation}
      end
    end
  end

  @doc false
  @spec match_delete(ETS.table_identifier(), ETS.match_pattern()) :: :ok | {:error, any()}
  def match_delete(table, pattern) do
    catch_error do
      catch_read_protected table do
        catch_table_not_found table do
          :ets.match_delete(table, pattern)
          :ok
        end
      end
    end
  end

  @doc false
  @spec match_object(ETS.table_identifier(), ETS.match_pattern()) ::
          {:ok, [tuple()]} | {:error, any()}
  def match_object(table, pattern) do
    catch_error do
      catch_read_protected table do
        catch_table_not_found table do
          matches = :ets.match_object(table, pattern)
          {:ok, matches}
        end
      end
    end
  end

  @doc false
  @spec match_object(ETS.table_identifier(), ETS.match_pattern(), non_neg_integer()) ::
          {:ok, {[tuple()], any()}} | {:error, any()}
  def match_object(table, pattern, limit) do
    catch_error do
      catch_read_protected table do
        catch_table_not_found table do
          case :ets.match_object(table, pattern, limit) do
            {x, :"$end_of_table"} -> {:ok, {x, :end_of_table}}
            {records, continuation} -> {:ok, {records, continuation}}
            :"$end_of_table" -> {:ok, {[], :end_of_table}}
          end
        end
      end
    end
  end

  @doc false
  @spec match_object(any()) :: {:ok, {[tuple()], any() | :end_of_table}} | {:error, any()}
  def match_object(continuation) do
    catch_error do
      try do
        case :ets.match_object(continuation) do
          {x, :"$end_of_table"} -> {:ok, {x, :end_of_table}}
          {records, continuation} -> {:ok, {records, continuation}}
          :"$end_of_table" -> {:ok, {[], :end_of_table}}
        end
      rescue
        ArgumentError ->
          {:error, :invalid_continuation}
      end
    end
  end

  @spec select(ETS.continuation()) ::
          {:ok, {[tuple()], ETS.continuation()} | ETS.end_of_table()} | {:error, any()}
  def select(continuation) do
    catch_error do
      catch_invalid_continuation continuation do
        matches = :ets.select(continuation)
        {:ok, matches}
      end
    end
  end

  @doc false
  @spec select(ETS.table_identifier(), ETS.match_spec()) :: {:ok, [tuple()]} | {:error, any()}
  def select(table, spec) when is_list(spec) do
    catch_error do
      catch_read_protected table do
        catch_invalid_select_spec spec do
          catch_table_not_found table do
            matches = :ets.select(table, spec)
            {:ok, matches}
          end
        end
      end
    end
  end

  @doc false
  @spec select(ETS.table_identifier(), ETS.match_spec(), limit :: integer) ::
          {:ok, {[tuple()], ETS.continuation()} | ETS.end_of_table()} | {:error, any()}
  def select(table, spec, limit) when is_list(spec) do
    catch_error do
      catch_read_protected table do
        catch_invalid_select_spec spec do
          catch_table_not_found table do
            matches = :ets.select(table, spec, limit)
            {:ok, matches}
          end
        end
      end
    end
  end

  @doc false
  @spec select_delete(ETS.table_identifier(), ETS.match_spec()) ::
          {:ok, non_neg_integer()} | {:error, any()}
  def select_delete(table, spec) when is_list(spec) do
    catch_error do
      catch_read_protected table do
        catch_invalid_select_spec spec do
          catch_table_not_found table do
            count = :ets.select_delete(table, spec)
            {:ok, count}
          end
        end
      end
    end
  end

  @doc false
  @spec has_key(ETS.table_identifier(), any()) :: {:ok, boolean()} | {:error, any()}
  def has_key(table, key) do
    catch_error do
      catch_read_protected table do
        catch_table_not_found table do
          {:ok, :ets.member(table, key)}
        end
      end
    end
  end

  @doc false
  @spec first(ETS.table_identifier()) :: {:ok, any()} | {:error, any()}
  def first(table) do
    catch_error do
      catch_read_protected table do
        catch_table_not_found table do
          case :ets.first(table) do
            :"$end_of_table" -> {:error, :empty_table}
            x -> {:ok, x}
          end
        end
      end
    end
  end

  @doc false
  @spec last(ETS.table_identifier()) :: {:ok, any()} | {:error, any()}
  def last(table) do
    catch_error do
      catch_read_protected table do
        catch_table_not_found table do
          case :ets.last(table) do
            :"$end_of_table" -> {:error, :empty_table}
            x -> {:ok, x}
          end
        end
      end
    end
  end

  @doc false
  @spec next(ETS.table_identifier(), any()) :: {:ok, any()} | {:error, any()}
  def next(table, key) do
    catch_error do
      catch_read_protected table do
        catch_table_not_found table do
          case :ets.next(table, key) do
            :"$end_of_table" -> {:error, :end_of_table}
            x -> {:ok, x}
          end
        end
      end
    end
  end

  @doc false
  @spec previous(ETS.table_identifier(), any()) :: {:ok, any()} | {:error, any()}
  def previous(table, key) do
    catch_error do
      catch_read_protected table do
        catch_table_not_found table do
          case :ets.prev(table, key) do
            :"$end_of_table" -> {:error, :start_of_table}
            x -> {:ok, x}
          end
        end
      end
    end
  end

  @doc false
  @spec delete(ETS.table_identifier(), any()) :: {:ok, any()} | {:error, any()}
  def delete(table, return) do
    catch_error do
      catch_write_protected table do
        catch_table_not_found table do
          :ets.delete(table)
          {:ok, return}
        end
      end
    end
  end

  @doc false
  @spec delete_records(ETS.table_identifier(), any(), any()) :: {:ok, any()} | {:error, any()}
  def delete_records(table, key, return) do
    catch_error do
      catch_write_protected table do
        catch_table_not_found table do
          :ets.delete(table, key)
          {:ok, return}
        end
      end
    end
  end

  @doc false
  @spec delete_all_records(ETS.table_identifier(), any()) :: {:ok, any()} | {:error, any()}
  def delete_all_records(table, return) do
    catch_error do
      catch_write_protected table do
        catch_table_not_found table do
          :ets.delete_all_objects(table)
          {:ok, return}
        end
      end
    end
  end

  @doc false
  @spec wrap_existing(ETS.table_identifier(), [table_types]) ::
          {:ok, {ETS.table_reference(), keyword()}} | {:error, any()}
  def wrap_existing(table, valid_types) do
    catch_error do
      catch_table_not_found table do
        case :ets.info(table) do
          :undefined ->
            {:error, :table_not_found}

          info ->
            if info[:type] in valid_types do
              {:ok, {info[:id], info}}
            else
              {:error, :invalid_type}
            end
        end
      end
    end
  end

  @doc false
  @spec update_element(ETS.table_identifier(), any(), tuple() | [tuple()]) ::
          boolean() | {:error, any()}
  def update_element(table, key, element_spec) do
    catch_error do
      catch_key_update table, element_spec do
        catch_positions_out_of_bounds table, key, element_spec do
          catch_write_protected table do
            catch_table_not_found table do
              :ets.update_element(table, key, element_spec)
            end
          end
        end
      end
    end
  end

  @spec give_away(ETS.table_identifier(), pid(), any(), any()) :: {:ok, any()} | {:error, any()}
  def give_away(table, pid, gift, return) do
    catch_error do
      catch_sender_not_table_owner table do
        catch_recipient_not_alive pid do
          catch_recipient_already_owns_table table, pid do
            catch_table_not_found table do
              :ets.give_away(table, pid, gift)
              {:ok, return}
            end
          end
        end
      end
    end
  end

  @doc false
  @spec accept(integer() | :infinity) ::
          {:ok, ETS.table_identifier(), pid(), any()} | {:error, :timeout}
  def accept(timeout) do
    receive do
      {:"ETS-TRANSFER", table, from, gift} ->
        {:ok, table, from, gift}
    after
      timeout ->
        {:error, :timeout}
    end
  end

  defmacro accept(id, table, from, state, do: contents) do
    quote do
      def handle_info(
            {:"ETS-TRANSFER", unquote(table), unquote(from), unquote(id)},
            unquote(state)
          ) do
        var!(unquote(table)) = unquote(table)
        unquote(contents)
      end
    end
  end
end
