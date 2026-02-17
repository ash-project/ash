defprotocol Ymlr.Encoder do
  @moduledoc ~S"""
  Protocol controlling how a value is encoded to YAML.

  ## Deriving

  The protocol allows leveraging the Elixir's `@derive` feature to simplify
  protocol implementation in trivial cases. Accepted options are:

    * `:only` - encodes only values of specified keys.
    * `:except` - If set to a list of fields, all struct fields except specified
      keys are encoded. If set to `:defaults`, all fields are encoded, except
      the ones equaling their default value as defined in the struct.

  By default all keys except the `:__struct__` key are encoded.

  ## Example

  Let's assume a presence of the following struct:

  ```
  defmodule Test do
    defstruct [:foo, :bar, :baz]
  end
  ```

  If we were to call `@derive Ymlr.Encoder` just before `defstruct`, an
  implementation similar to the following implementation would be generated:

  ```
  defimpl Ymlr.Encoder, for: Test do
    def encode(data, indent_level) do
      data
      |> Map.take(unquote([:foo, :bar, :baz]))
      |> Ymlr.Encode.map(indent_level)
    end
  end
  ```

  ### Limit fields using the `only` Option

  We can limit the fields being encoded using the `:only` option:

  ```
  defmodule Test do
    @derive {Ymlr.Encoder, only: [:foo]}
    defstruct [:foo, :bar, :baz]
  end
  ```

  This would generate an implementation similar to the following:

  ```
  defimpl Ymlr.Encoder, for: Test do
    def encode(data, indent_level) do
      data
      |> Map.take(unquote([:foo]))
      |> Ymlr.Encode.map(indent_level)
    end
  end
  ```

  ### Exclude fields using the `except` Option

  We can exclude the fields being encoded using the `:except` option:

  ```
  defmodule Test do
    @derive {Ymlr.Encoder, except: [:foo]}
    defstruct [:foo, :bar, :baz]
  end
  ```

  This would generate an implementation similar to the following:

  ```
  defimpl Ymlr.Encoder, for: Test do
    def encode(data, indent_level) do
      data
      |> Map.take(unquote([:bar, :baz]))
      |> Ymlr.Encode.map(indent_level)
    end
  end
  ```

  We can exclude the fields being left at their defaults by passing `except:
  :defaults`:

  ```
  defmodule TestExceptDefaults do
    @derive {Ymlr.Encoder, except: :defaults}
    defstruct [:foo, bar: 1, baz: :ok]
  end
  ```

  This would generate an implementation similar to the following:

  ```
  iex> Ymlr.document!(%TestExceptDefaults{foo: 1, bar: 1, baz: :error})
  "baz: error\nfoo: 1"
  ```
  """

  @fallback_to_any true

  @type opts :: keyword()

  @doc """
  Encodes the given data to YAML.
  """
  @spec encode(data :: term(), indent_level :: integer(), opts :: opts()) :: iodata()
  def encode(data, indent_level, opts)
end

defimpl Ymlr.Encoder, for: Any do
  defmacro __deriving__(module, struct, opts) do
    if opts[:except] === :defaults do
      defaults =
        struct
        |> Map.from_struct()
        |> MapSet.new()
        |> Macro.escape()

      quote do
        defimpl Ymlr.Encoder, for: unquote(module) do
          def encode(data, indent_level, opts) do
            data
            |> Map.from_struct()
            |> MapSet.new()
            |> MapSet.difference(unquote(defaults))
            |> Map.new()
            |> Ymlr.Encode.map(indent_level, opts)
          end
        end
      end
    else
      fields = fields_to_encode(struct, opts)

      quote do
        defimpl Ymlr.Encoder, for: unquote(module) do
          def encode(data, indent_level, opts) do
            data
            |> Map.take(unquote(fields))
            |> Ymlr.Encode.map(indent_level, opts)
          end
        end
      end
    end
  end

  def encode(%_{} = struct, _level, _opts) do
    raise Protocol.UndefinedError,
      protocol: @protocol,
      value: struct,
      description: """
      Ymlr.Encoder protocol must always be explicitly implemented.
      If you own the struct, you can derive the implementation specifying \
      which fields should be encoded to YAML:
          @derive {Ymlr.Encoder, only: [....]}
          defstruct ...
      It is also possible to encode all fields, although this should be \
      used carefully to avoid accidentally leaking private information \
      when new fields are added:
          @derive Ymlr.Encoder
          defstruct ...
      Finally, if you don't own the struct you want to encode to YAML, \
      you may use Protocol.derive/3 placed outside of any module:
          Protocol.derive(Ymlr.Encoder, NameOfTheStruct, only: [...])
          Protocol.derive(Ymlr.Encoder, NameOfTheStruct)
      """
  end

  def encode(value, _level, _opts) do
    raise Protocol.UndefinedError,
      protocol: @protocol,
      value: value,
      description: "Ymlr.Encoder protocol must always be explicitly implemented"
  end

  defp fields_to_encode(struct, opts) do
    fields = Map.keys(struct)

    cond do
      only = Keyword.get(opts, :only) ->
        case only -- fields do
          [] ->
            only

          error_keys ->
            raise ArgumentError,
                  "`:only` specified keys (#{inspect(error_keys)}) that are not defined in defstruct: " <>
                    "#{inspect(fields -- [:__struct__])}"
        end

      except = Keyword.get(opts, :except) ->
        case except -- fields do
          [] ->
            fields -- [:__struct__ | except]

          error_keys ->
            raise ArgumentError,
                  "`:except` specified keys (#{inspect(error_keys)}) that are not defined in defstruct: " <>
                    "#{inspect(fields -- [:__struct__])}"
        end

      true ->
        fields -- [:__struct__]
    end
  end
end

defimpl Ymlr.Encoder, for: Map do
  def encode(data, indent_level, opts), do: Ymlr.Encode.map(data, indent_level, opts)
end

defimpl Ymlr.Encoder, for: [Date, Time, NaiveDateTime] do
  def encode(data, _level, _opts), do: @for.to_iso8601(data)
end

defimpl Ymlr.Encoder, for: DateTime do
  def encode(data, _level, _opts) do
    data |> DateTime.shift_zone!("Etc/UTC") |> DateTime.to_iso8601()
  end
end

defimpl Ymlr.Encoder, for: List do
  def encode(data, indent_level, opts), do: Ymlr.Encode.list(data, indent_level, opts)
end

defimpl Ymlr.Encoder, for: Tuple do
  def encode(data, indent_level, opts) do
    Ymlr.Encode.list(Tuple.to_list(data), indent_level, opts)
  end
end

defimpl Ymlr.Encoder, for: Atom do
  def encode(data, _indent_level, _opts), do: Ymlr.Encode.atom(data)
end

defimpl Ymlr.Encoder, for: BitString do
  def encode(binary, indent_level, _opts) when is_binary(binary) do
    Ymlr.Encode.string(binary, indent_level)
  end

  def encode(bitstring, _indent_level, _opts) do
    raise Protocol.UndefinedError,
      protocol: @protocol,
      value: bitstring,
      description: "cannot encode a bitstring to YAML"
  end
end

defimpl Ymlr.Encoder, for: Integer do
  def encode(data, _indent_level, _opts), do: Ymlr.Encode.number(data)
end

defimpl Ymlr.Encoder, for: Float do
  def encode(data, _indent_level, _opts), do: Ymlr.Encode.number(data)
end

if Code.ensure_loaded?(Decimal) do
  defimpl Ymlr.Encoder, for: Decimal do
    def encode(data, _indent_level, _opts) do
      # silence the xref warning
      Decimal.to_string(data)
    end
  end
end
