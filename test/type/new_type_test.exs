# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.NewTypeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

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
      d.year * 365 * 24 * 3600 +
        d.month * 30 * 24 * 3600 +
        d.week * 7 * 24 * 3600 +
        d.day * 24 * 3600 +
        d.hour * 3600 +
        d.minute * 60 +
        d.second
    end

    @impl Ash.Type
    def apply_constraints(value, constraints) do
      Enum.reduce(constraints, [], fn constraint, errors ->
        case apply_constraint(value, constraint) do
          :ok -> errors
          {:error, error} -> [error | errors]
        end
      end)
      |> case do
        [] -> {:ok, value}
        errors -> {:error, errors}
      end
    end

    defp apply_constraint(value, {:min, min}) do
      if to_seconds(value) < min,
        do: {:error, message: "must be at least %{min} seconds", min: min},
        else: :ok
    end

    defp apply_constraint(value, {:max, max}) do
      if to_seconds(value) > max,
        do: {:error, message: "must be at most %{max} seconds", max: max},
        else: :ok
    end
  end

  defmodule DurationRecord do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :bounded, BoundedDuration,
        constraints: [min: 60, max: 3600],
        public?: true

      attribute :bounded_list, {:array, BoundedDuration},
        constraints: [items: [min: 60, max: 3600]],
        public?: true
    end
  end

  describe "BoundedDuration (duration with min/max)" do
    test "exposes custom constraints on a type that natively has none" do
      assert Ash.Type.Duration.constraints() == []
      keys = BoundedDuration.constraints() |> Keyword.keys()
      assert :min in keys
      assert :max in keys
    end

    test "accepts a valid duration" do
      ten_min = Duration.new!(minute: 10)

      record =
        DurationRecord
        |> Ash.Changeset.for_create(:create, %{bounded: ten_min})
        |> Ash.create!()

      assert record.bounded == ten_min
    end

    test "rejects a duration below the minimum" do
      thirty_sec = Duration.new!(second: 30)

      assert {:error, %Ash.Error.Invalid{} = error} =
               DurationRecord
               |> Ash.Changeset.for_create(:create, %{bounded: thirty_sec})
               |> Ash.create()

      assert Exception.message(error) =~ "must be at least"
    end

    test "rejects a duration above the maximum" do
      two_hours = Duration.new!(hour: 2)

      assert {:error, %Ash.Error.Invalid{} = error} =
               DurationRecord
               |> Ash.Changeset.for_create(:create, %{bounded: two_hours})
               |> Ash.create()

      assert Exception.message(error) =~ "must be at most"
    end

    test "accepts a valid duration array" do
      ten_min = Duration.new!(minute: 10)
      thirty_min = Duration.new!(minute: 30)

      record =
        DurationRecord
        |> Ash.Changeset.for_create(:create, %{bounded_list: [ten_min, thirty_min]})
        |> Ash.create!()

      assert record.bounded_list == [ten_min, thirty_min]
    end

    test "rejects an invalid duration in array" do
      ten_min = Duration.new!(minute: 10)
      thirty_sec = Duration.new!(second: 30)

      assert {:error, %Ash.Error.Invalid{} = error} =
               DurationRecord
               |> Ash.Changeset.for_create(:create, %{bounded_list: [ten_min, thirty_sec]})
               |> Ash.create()

      assert Exception.message(error) =~ "must be at least"
    end

    test "rejects a duration above the maximum in array" do
      two_hours = Duration.new!(hour: 2)

      assert {:error, %Ash.Error.Invalid{} = error} =
               DurationRecord
               |> Ash.Changeset.for_create(:create, %{bounded_list: [two_hours]})
               |> Ash.create()

      assert Exception.message(error) =~ "must be at most"
    end

    test "array apply_constraints enforces custom constraints on each item" do
      {:ok, constraints} = Ash.Type.init(BoundedDuration, min: 60, max: 3600)
      thirty_sec = Duration.new!(second: 30)
      ten_min = Duration.new!(minute: 10)

      assert {:error, _} =
               Ash.Type.apply_constraints(
                 {:array, BoundedDuration},
                 [ten_min, thirty_sec],
                 items: constraints
               )
    end

    test "unknown constraint keys are rejected" do
      assert {:error, error} = Ash.Type.init(BoundedDuration, unit: :millisecond)
      assert error =~ "Unknown options"
      assert error =~ "unit"
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

  defmodule EmailRecord do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :email, EmailAddress,
        constraints: [allowed_domains: ["company.com", "corp.co"]],
        public?: true

      attribute :emails, {:array, EmailAddress},
        constraints: [items: [allowed_domains: ["company.com", "corp.co"]]],
        public?: true
    end
  end

  describe "EmailAddress (string with domain restriction)" do
    test "accepts a valid email at an allowed domain" do
      record =
        EmailRecord
        |> Ash.Changeset.for_create(:create, %{email: "user@company.com"})
        |> Ash.create!()

      assert record.email == "user@company.com"
    end

    test "rejects a valid email at a disallowed domain" do
      assert {:error, %Ash.Error.Invalid{} = error} =
               EmailRecord
               |> Ash.Changeset.for_create(:create, %{email: "user@gmail.com"})
               |> Ash.create()

      assert Exception.message(error) =~ "must be an address at one of"
    end

    test "rejects strings that fail the baked-in regex" do
      assert {:error, %Ash.Error.Invalid{} = error} =
               EmailRecord
               |> Ash.Changeset.for_create(:create, %{email: "not-an-email"})
               |> Ash.create()

      assert Exception.message(error) =~ "must match the pattern"
    end

    test "accepts a valid email array at allowed domains" do
      record =
        EmailRecord
        |> Ash.Changeset.for_create(:create, %{
          emails: ["user@company.com", "admin@corp.co"]
        })
        |> Ash.create!()

      assert record.emails == ["user@company.com", "admin@corp.co"]
    end

    test "rejects a disallowed domain in email array" do
      assert {:error, %Ash.Error.Invalid{} = error} =
               EmailRecord
               |> Ash.Changeset.for_create(:create, %{
                 emails: ["user@company.com", "user@gmail.com"]
               })
               |> Ash.create()

      assert Exception.message(error) =~ "must be an address at one of"
    end

    test "rejects an invalid email format in array" do
      assert {:error, %Ash.Error.Invalid{} = error} =
               EmailRecord
               |> Ash.Changeset.for_create(:create, %{emails: ["not-an-email"]})
               |> Ash.create()

      assert Exception.message(error) =~ "must match the pattern"
    end

    test "array apply_constraints enforces subtype regex on each item" do
      {:ok, constraints} = Ash.Type.init(EmailAddress, allowed_domains: ["company.com"])

      # "has space@company.com" passes domain check but fails subtype regex
      assert {:error, _} =
               Ash.Type.apply_constraints(
                 {:array, EmailAddress},
                 ["has space@company.com"],
                 items: constraints
               )
    end

    test "array apply_constraints accepts valid values" do
      {:ok, constraints} = Ash.Type.init(EmailAddress, allowed_domains: ["company.com"])

      assert {:ok, ["user@company.com"]} =
               Ash.Type.apply_constraints(
                 {:array, EmailAddress},
                 ["user@company.com"],
                 items: constraints
               )
    end

    test "subtype string constraints still available alongside custom" do
      keys = EmailAddress.constraints() |> Keyword.keys()
      assert :allowed_domains in keys
      assert :max_length in keys
      assert :match in keys
    end
  end
end
