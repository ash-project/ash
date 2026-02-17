defmodule ETS.KeyValueSet do
  @moduledoc """
  The Key Value Set is an extension of `ETS.Set` which abstracts the concept of tuple records
  away, replacing it with the standard concept of key/value. Behind the scenes, the set stores
  its records as {key, value}.

  ## Examples

      iex> {:ok, kvset} = KeyValueSet.new()
      iex> KeyValueSet.put(kvset, :my_key, :my_val)
      iex> KeyValueSet.get(kvset, :my_key)
      {:ok, :my_val}

  `KeyValueSet` implements [`Access`] _behaviour_.

  ## Examples

      iex> set =
      ...>   KeyValueSet.new!()
      ...>   |> KeyValueSet.put!(:k1, :v1)
      ...>   |> KeyValueSet.put!(:k2, :v2)
      ...>   |> KeyValueSet.put!(:k3, :v3)
      iex> get_in(set, [:k1])
      :v1
      iex> get_in(set, [:z])
      nil
      iex> with {:v2, set} <-
      ...>   pop_in(set, [:k2]), do: KeyValueSet.to_list!(set)
      [k3: :v3, k1: :v1]
      iex> with {nil, set} <- pop_in(set, [:z]), do: KeyValueSet.to_list!(set)
      [k3: :v3, k1: :v1]
      iex> with {:v1, set} <-
      ...>     get_and_update_in(set, [:k1], &{&1, :v42}),
      ...>   do: KeyValueSet.to_list!(set)
      [k3: :v3, k1: :v42]
      iex> with {:v42, set} <-
      ...>     get_and_update_in(set, [:k1], fn _ -> :pop end),
      ...>   do: KeyValueSet.to_list!(set)
      [k3: :v3]

  """
  use ETS.Utils
  use ETS.KeyValueSet.Macros

  @behaviour Access

  alias ETS.Base
  alias ETS.KeyValueSet
  alias ETS.Set

  @type t :: %__MODULE__{
          set: Set.t()
        }

  @type set_options :: [ETS.Base.option() | {:ordered, boolean()}]

  defstruct set: nil

  @doc """
  Creates new Key Value Set module with the specified options.

  Possible Options can be found in `ETS.Set` with the difference that specifying a `keypos`
  will result in an error.

  ## Examples

      iex> {:ok, kvset} = KeyValueSet.new(ordered: true,read_concurrency: true, compressed: false)
      iex> KeyValueSet.info!(kvset)[:read_concurrency]
      true

      # Named :ets tables via the name keyword
      iex> {:ok, kvset} = KeyValueSet.new(name: :my_ets_table)
      iex> KeyValueSet.info!(kvset)[:name]
      :my_ets_table

  """
  @spec new(set_options) :: {:error, any()} | {:ok, KeyValueSet.t()}
  def new(opts \\ []) when is_list(opts) do
    with(
      {:keypos, false} <- {:keypos, Keyword.has_key?(opts, :keypos)},
      {:ok, set} <- Set.new(opts)
    ) do
      {:ok, %KeyValueSet{set: set}}
    else
      {:keypos, true} -> {:error, {:invalid_option, {:keypos, Keyword.get(opts, :keypos)}}}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Same as `new/1` but unwraps or raises on error.
  """
  @spec new!(set_options) :: KeyValueSet.t()
  def new!(opts \\ []), do: unwrap_or_raise(new(opts))

  @doc """
  Wraps an existing :ets :set or :ordered_set in a KeyValueSet struct.

  ## Examples

      iex> :ets.new(:my_ets_table, [:set, :named_table])
      iex> {:ok, set} = KeyValueSet.wrap_existing(:my_ets_table)
      iex> KeyValueSet.info!(set)[:name]
      :my_ets_table

  """
  @spec wrap_existing(ETS.table_identifier()) :: {:ok, KeyValueSet.t()} | {:error, any()}
  def wrap_existing(table_identifier) do
    with(
      {:ok, set} <- Set.wrap_existing(table_identifier),
      {:ok, info} <- Set.info(set),
      {:keypos, true} <- {:keypos, info[:keypos] == 1}
    ) do
      {:ok, %KeyValueSet{set: set}}
    else
      {:keypos, false} -> {:error, :invalid_keypos}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Same as `wrap_existing/1` but unwraps or raises on error.
  """
  @spec wrap_existing!(ETS.table_identifier()) :: KeyValueSet.t()
  def wrap_existing!(table_identifier), do: unwrap_or_raise(wrap_existing(table_identifier))

  @doc """
  Puts given value into table for given key.

  ## Examples

      iex> kvset = KeyValueSet.new!(ordered: true)
      iex> {:ok, kvset} = KeyValueSet.put(kvset, :a, :b)
      iex> KeyValueSet.get!(kvset, :a)
      :b

  """
  def put(%KeyValueSet{set: set} = key_value_set, key, value) do
    case Set.put(set, {key, value}) do
      {:ok, _} -> {:ok, key_value_set}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Same as `put/3` but unwraps or raises on error.
  """
  @spec put!(KeyValueSet.t(), any(), any()) :: KeyValueSet.t()
  def put!(%KeyValueSet{} = key_value_set, key, value),
    do: unwrap_or_raise(put(key_value_set, key, value))

  @doc """
  Same as `put/3` but doesn't put record if the key already exists.

  ## Examples

      iex> set = KeyValueSet.new!(ordered: true)
      iex> {:ok, _} = KeyValueSet.put_new(set, :a, :b)
      iex> {:ok, _} = KeyValueSet.put_new(set, :a, :c) # skips due toduplicate :a key
      iex> KeyValueSet.to_list!(set)
      [{:a, :b}]

  """
  @spec put_new(KeyValueSet.t(), any(), any()) :: {:ok, KeyValueSet.t()} | {:error, any()}
  def put_new(%KeyValueSet{set: set} = key_value_set, key, value) do
    case Set.put_new(set, {key, value}) do
      {:ok, _} -> {:ok, key_value_set}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Same as `put_new/3` but unwraps or raises on error.
  """
  @spec put_new!(KeyValueSet.t(), any(), any()) :: KeyValueSet.t()
  def put_new!(%KeyValueSet{} = key_value_set, key, value),
    do: unwrap_or_raise(put_new(key_value_set, key, value))

  @doc """
  Returns value for specified key or the provided default (nil if not specified) if no record found.

  ## Examples

      iex> KeyValueSet.new!()
      iex> |> KeyValueSet.put!(:a, :b)
      iex> |> KeyValueSet.put!(:c, :d)
      iex> |> KeyValueSet.put!(:e, :f)
      iex> |> KeyValueSet.get(:c)
      {:ok, :d}

  """
  @spec get(KeyValueSet.t(), any(), any()) :: {:ok, any()} | {:error, any()}
  def get(%KeyValueSet{set: set}, key, default \\ nil) do
    case Set.get(set, key, default) do
      {:ok, {_, value}} -> {:ok, value}
      {:ok, ^default} -> {:ok, default}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Same as `get/3` but unwraps or raises on error
  """
  @spec get!(KeyValueSet.t(), any(), any()) :: any()
  def get!(%KeyValueSet{} = key_value_set, key, default \\ nil),
    do: unwrap_or_raise(get(key_value_set, key, default))

  @doc """
  Deletes record with specified key in specified Set.

  ## Examples

      iex> set = KeyValueSet.new!()
      iex> KeyValueSet.put(set, :a, :b)
      iex> KeyValueSet.delete(set, :a)
      iex> KeyValueSet.get!(set, :a)
      nil

  """
  @spec delete(KeyValueSet.t(), any()) :: {:ok, KeyValueSet.t()} | {:error, any()}
  def delete(%KeyValueSet{set: set}, key) do
    with {:ok, %Set{table: table}} <- Set.delete(set, key),
         do: KeyValueSet.wrap_existing(table)
  end

  @doc """
  Same as `delete/2` but unwraps or raises on error.
  """
  @spec delete!(KeyValueSet.t(), any()) :: KeyValueSet.t()
  def delete!(%KeyValueSet{} = set, key),
    do: unwrap_or_raise(delete(set, key))

  @doc """
  Deletes all records in specified Set.

  ## Examples

      iex> set = KeyValueSet.new!()
      iex> set
      iex> |> KeyValueSet.put!(:a, :d)
      iex> |> KeyValueSet.put!(:b, :d)
      iex> |> KeyValueSet.put!(:c, :d)
      iex> |> KeyValueSet.to_list!()
      [c: :d, b: :d, a: :d]
      iex> KeyValueSet.delete_all(set)
      iex> KeyValueSet.to_list!(set)
      []

  """
  @spec delete_all(KeyValueSet.t()) :: {:ok, KeyValueSet.t()} | {:error, any()}
  def delete_all(%KeyValueSet{set: set}) do
    with {:ok, %Set{table: table}} <- Set.delete_all(set),
         do: KeyValueSet.wrap_existing(table)
  end

  @doc """
  Same as `delete_all/1` but unwraps or raises on error.
  """
  @spec delete_all!(KeyValueSet.t()) :: KeyValueSet.t()
  def delete_all!(%KeyValueSet{} = set),
    do: unwrap_or_raise(delete_all(set))

  def info(key_value_set, force_update \\ false)
  def info!(key_value_set, force_update \\ false)

  @doc """
  Transfers ownership of a KeyValueSet to another process.

  ## Examples

      iex> kv_set = KeyValueSet.new!()
      iex> receiver_pid = spawn(fn -> KeyValueSet.accept() end)
      iex> KeyValueSet.give_away(kv_set, receiver_pid)
      {:ok, kv_set}

      iex> kv_set = KeyValueSet.new!()
      iex> dead_pid = ETS.TestUtils.dead_pid()
      iex> KeyValueSet.give_away(kv_set, dead_pid)
      {:error, :recipient_not_alive}

  """
  @spec give_away(KeyValueSet.t(), pid(), any()) :: {:ok, KeyValueSet.t()} | {:error, any()}
  def give_away(%KeyValueSet{set: set}, pid, gift \\ []) do
    with {:ok, set} <- Set.give_away(set, pid, gift),
         do: {:ok, %KeyValueSet{set: set}}
  end

  @doc """
  Same as `give_away/3` but unwraps or raises on error.
  """
  @spec give_away!(KeyValueSet.t(), pid(), any()) :: KeyValueSet.t()
  def give_away!(%KeyValueSet{} = kv_set, pid, gift \\ []),
    do: unwrap_or_raise(give_away(kv_set, pid, gift))

  @doc """
  Waits to accept ownership of a table after it is given away.  Successful receipt will
  return `{:ok, %{kv_set: kv_set, from: from, gift: gift}}` where `from` is the pid of
  the previous owner, and `gift` is any additional metadata sent with the table.

  A timeout may be given in milliseconds, which will return `{:error, :timeout}` if reached.

  See `give_away/3` for more information.
  """
  @spec accept() :: {:ok, KeyValueSet.t(), pid(), any()} | {:error, any()}
  def accept(timeout \\ :infinity) do
    with {:ok, %{set: set, from: from, gift: gift}} <- Set.accept(timeout),
         do: {:ok, %{kv_set: %KeyValueSet{set: set}, from: from, gift: gift}}
  end

  delegate_to_set :info, 2, ret: keyword(), second_param_type: boolean() do
    "Returns info on set"
  end

  delegate_to_set :get_table, 1, ret: ETS.table_reference(), can_raise: false do
    "Returns underlying `:ets` table reference"
  end

  delegate_to_set(:first, 1, do: "Returns first key in KeyValueSet")
  delegate_to_set(:last, 1, do: "Returns last key in KeyValueSet")
  delegate_to_set(:next, 2, do: "Returns next key in KeyValueSet")
  delegate_to_set(:previous, 2, do: "Returns previous key in KeyValueSet")
  delegate_to_set(:has_key, 2, do: "Determines if specified key exists in KeyValueSet")
  delegate_to_set(:delete, 1, do: "Deletes KeyValueSet")
  delegate_to_set(:to_list, 1, do: "Returns contents of table as a list")

  ### Access behaviour implementation

  @doc false
  @doc since: "0.7.0"
  @impl true
  def fetch(set, key) do
    case get(set, key) do
      {:ok, result} -> {:ok, result}
      _ -> :error
    end
  end

  @doc false
  @doc since: "0.7.0"
  @impl true
  def get_and_update(set, key, function) do
    value =
      case fetch(set, key) do
        {:ok, value} -> value
        _ -> nil
      end

    case function.(value) do
      :pop -> {value, delete!(set, key)}
      {^value, updated} -> {value, put!(set, key, updated)}
    end
  end

  @doc false
  @doc since: "0.7.0"
  @impl true
  def pop(set, key) do
    case get(set, key) do
      {:ok, value} -> {value, delete!(set, key)}
      _ -> {nil, set}
    end
  end

  @doc """
  For processes which may receive ownership of a KeyValueSet unexpectedly - either via
  `give_away/3` or by being named the KeyValueSet's heir (see `new/1`) - the module should
  include at least one `accept` clause.  For example, if we want a server to inherit
  KeyValueSets after their previous owner dies:

  ```
  defmodule Receiver do
    use GenServer
    alias ETS.KeyValueSet
    require ETS.KeyValueSet

    ...

    KeyValueSet.accept :owner_crashed, kv_set, _from, state do
      new_state = Map.update!(state, :crashed_sets, &[kv_set | &1])
      {:noreply, new_state}
    end
  ```

  The first argument is a unique identifier which should match either the "heir_data"
  in `new/1`, or the "gift" in `give_away/3`.
  The other arguments declare the variables which may be used in the `do` block:
  the received KeyValueSet, the pid of the previous owner, and the current state of the process.

  The return value should be in the form {:noreply, new_state}, or one of the similar
  returns expected by `handle_info`/`handle_cast`.
  """
  defmacro accept(id, table, from, state, do: contents) do
    quote do
      require Base

      Base.accept unquote(id), unquote(table), unquote(from), unquote(state) do
        var!(unquote(table)) = KeyValueSet.wrap_existing!(unquote(table))
        unquote(contents)
      end
    end
  end
end
