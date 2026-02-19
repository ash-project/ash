# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.NewTypeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule SSN do
    use Ash.Type.NewType,
      subtype_of: :string,
      constraints: [match: ~r/^(?!0{3})(?!6{3})[0-8]\d{2}-(?!0{2})\d{2}-(?!0{4})\d{4}$/]
  end

  defmodule CustomMap do
    use Ash.Type.NewType,
      subtype_of: :map,
      constraints: [
        fields: [
          foo: [
            type: :string
          ],
          bar: [
            type: :integer
          ]
        ]
      ]
  end

  defmodule CustomMapWithArray do
    use Ash.Type.NewType,
      subtype_of: :map,
      constraints: [
        fields: [
          custom_map: [
            type: {:array, CustomMap}
          ]
        ]
      ]
  end

  defmodule NativeBird do
    defstruct [:name, :description]

    use Ash.Type.NewType,
      subtype_of: :struct,
      constraints: [
        instance_of: __MODULE__,
        fields: [
          name: [type: :string, allow_nil?: false],
          description: [type: :string, allow_nil?: false]
        ]
      ]
  end

  defmodule LazyNativeBird do
    use Ash.Type.NewType,
      subtype_of: :struct,
      lazy_init?: true,
      constraints: [
        instance_of: NativeBird,
        fields: [
          name: [type: :string, allow_nil?: false],
          description: [type: :string, allow_nil?: false]
        ]
      ]
  end

  test "constraints return the initialized constraints" do
    {:ok, constraints} = Ash.Type.init(NativeBird, [])
    assert constraints == Ash.Type.NewType.constraints(NativeBird, constraints)
  end

  test "constraints return the initialized constraints for lazy initialization" do
    {:ok, constraints} = Ash.Type.init(NativeBird, [])

    assert Enum.sort(constraints) ==
             Enum.sort(Ash.Type.NewType.constraints(LazyNativeBird, constraints))
  end

  test "it handles simple types" do
    assert {:ok, "123-45-6789"} = Ash.Type.cast_input(SSN, "123-45-6789")

    assert {:ok, nil} = Ash.Type.cast_input(SSN, nil)
  end

  test "it applies the provided constraints" do
    assert {:error,
            message: "must match the pattern %{regex}",
            regex: "~r/^(?!0{3})(?!6{3})[0-8]\\d{2}-(?!0{2})\\d{2}-(?!0{4})\\d{4}$/"} =
             Ash.Type.cast_input(SSN, "hello world")
  end

  test "it casts maps correctly" do
    assert {:ok, %{foo: "baz", bar: 42}} ==
             Ash.Type.cast_input(CustomMap, %{"foo" => "baz", "bar" => "42", "other" => "ignored"})
  end

  test "it casts array of maps correctly" do
    assert {:ok, [%{foo: "baz", bar: 42}]} ==
             Ash.Type.cast_input({:array, CustomMap}, [
               %{"foo" => "baz", "bar" => "42", "other" => "ignored"}
             ])
  end

  test "it casts a nested array of maps correctly" do
    assert {:ok, %{custom_map: [%{foo: "baz", bar: 42}]}} ==
             Ash.Type.cast_input(
               CustomMapWithArray,
               %{"custom_map" => [%{"foo" => "baz", "bar" => "42", "other" => "ignored"}]}
             )
  end

  test "returns error for invalid keys" do
    foo = {:foo, [type: :string]}
    bar = {:bar, [type: :integer]}

    assert_raise ArgumentError, ~r/Unknown options given/, fn ->
      defmodule InvalidTypeBySubtype do
        use Ash.Type.NewType,
          subtype_of: :map,
          something: :invalid,
          constraints: [fields: [foo, bar]] ++ [{:something, :invalid}]
      end
    end
  end

  defmodule BoundedDuration do
    @moduledoc """
    A duration type with min/max constraints. Ash.Type.Duration defines no
    constraints at all, so this demonstrates adding constraints to a base type
    that has none.
    """
    use Ash.Type.NewType, subtype_of: :duration

    @impl Ash.Type
    def constraints do
      [
        min: [
          type: :pos_integer,
          doc: "Minimum duration in seconds"
        ],
        max: [
          type: :pos_integer,
          doc: "Maximum duration in seconds"
        ]
      ]
    end

    defp to_seconds(%Duration{} = d) do
      d.hour * 3600 + d.minute * 60 + d.second
    end

    @impl Ash.Type
    def apply_constraints(value, constraints) do
      seconds = to_seconds(value)

      errors =
        Enum.reduce(constraints, [], fn
          {:min, min}, errors ->
            if seconds < min do
              [[message: "must be at least %{min} seconds", min: min] | errors]
            else
              errors
            end

          {:max, max}, errors ->
            if seconds > max do
              [[message: "must be at most %{max} seconds", max: max] | errors]
            else
              errors
            end

          _, errors ->
            errors
        end)

      case errors do
        [] -> {:ok, value}
        errors -> {:error, errors}
      end
    end
  end

  describe "BoundedDuration (duration with min/max)" do
    test "exposes custom constraints on a type that natively has none" do
      assert Ash.Type.Duration.constraints() == []
      keys = BoundedDuration.constraints() |> Keyword.keys()
      assert :min in keys
      assert :max in keys
    end

    test "enforces minimum duration" do
      {:ok, constraints} = Ash.Type.init(BoundedDuration, min: 60)
      ten_min = Duration.new!(minute: 10)
      thirty_sec = Duration.new!(second: 30)

      assert {:ok, ^ten_min} = Ash.Type.apply_constraints(BoundedDuration, ten_min, constraints)
      assert {:error, _} = Ash.Type.apply_constraints(BoundedDuration, thirty_sec, constraints)
    end

    test "enforces maximum duration" do
      {:ok, constraints} = Ash.Type.init(BoundedDuration, max: 3600)
      thirty_min = Duration.new!(minute: 30)
      two_hours = Duration.new!(hour: 2)

      assert {:ok, ^thirty_min} =
               Ash.Type.apply_constraints(BoundedDuration, thirty_min, constraints)

      assert {:error, _} = Ash.Type.apply_constraints(BoundedDuration, two_hours, constraints)
    end

    test "cast_input still delegates to duration" do
      assert {:ok, nil} = Ash.Type.cast_input(BoundedDuration, nil)
    end

    test "unknown constraint keys are rejected" do
      assert {:error, _} = Ash.Type.init(BoundedDuration, unit: :millisecond)
    end
  end

  defmodule EmailAddress do
    @moduledoc """
    A string type that normalizes and validates email addresses beyond
    what a regex match alone can express.
    """
    use Ash.Type.NewType,
      subtype_of: :string,
      constraints: [
        match: ~r/^[^\s@]+@[^\s@]+$/
      ]

    @impl Ash.Type
    def constraints do
      Keyword.merge(super(),
        allowed_domains: [
          type: {:list, :string},
          doc: "If set, only addresses at these domains are accepted"
        ]
      )
    end

    @impl Ash.Type
    def apply_constraints(value, constraints) do
      case Keyword.get(constraints, :allowed_domains) do
        nil ->
          {:ok, value}

        domains ->
          domain = value |> String.split("@") |> List.last() |> String.downcase()

          if domain in domains do
            {:ok, value}
          else
            {:error,
             message: "must be an address at one of: %{domains}",
             domains: Enum.join(domains, ", ")}
          end
      end
    end
  end

  describe "EmailAddress (string with domain restriction)" do
    test "casts valid emails using the subtype's regex constraint" do
      assert {:ok, "user@example.com"} = Ash.Type.cast_input(EmailAddress, "user@example.com")
    end

    test "rejects strings that fail the baked-in regex" do
      assert {:error, _} = Ash.Type.cast_input(EmailAddress, "not-an-email")
    end

    test "restricts to allowed_domains when set" do
      {:ok, constraints} =
        Ash.Type.init(EmailAddress, allowed_domains: ["company.com", "corp.co"])

      assert {:ok, _} =
               Ash.Type.apply_constraints(EmailAddress, "user@company.com", constraints)

      assert {:error, _} =
               Ash.Type.apply_constraints(EmailAddress, "user@gmail.com", constraints)
    end

    test "allows any domain when allowed_domains is not set" do
      {:ok, constraints} = Ash.Type.init(EmailAddress, [])

      assert {:ok, _} =
               Ash.Type.apply_constraints(EmailAddress, "anyone@anywhere.org", constraints)
    end

    test "subtype string constraints still available alongside custom" do
      keys = EmailAddress.constraints() |> Keyword.keys()
      assert :allowed_domains in keys
      assert :max_length in keys
      assert :match in keys
    end
  end
end
