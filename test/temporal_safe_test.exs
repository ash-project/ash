# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.TemporalSafeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Error.Framework.NotTemporalSafe

  @as_of ~U[2020-06-15 12:00:00.000000Z]

  defmodule UnsafeChange do
    @moduledoc false
    use Ash.Resource.Change

    def change(changeset, _opts, _context), do: changeset
  end

  defmodule SafeChange do
    @moduledoc false
    use Ash.Resource.Change

    def temporal_safe?(_opts), do: true
    def change(changeset, _opts, _context), do: changeset
  end

  # Declares nothing itself, but is listed in `:temporal_safe_modules` in config/config.exs.
  defmodule ConfiguredSafeChange do
    @moduledoc false
    use Ash.Resource.Change

    def change(changeset, _opts, _context), do: changeset
  end

  defmodule OptsDependentChange do
    @moduledoc false
    use Ash.Resource.Change

    def temporal_safe?(opts), do: opts[:safe?] == true
    def change(changeset, _opts, _context), do: changeset
  end

  defmodule UnsafeAtomicChange do
    @moduledoc false
    use Ash.Resource.Change

    def atomic(_changeset, _opts, _context), do: {:atomic, %{}}
  end

  defmodule SafeAtomicChange do
    @moduledoc false
    use Ash.Resource.Change

    def temporal_safe?(_opts), do: true
    def atomic(_changeset, _opts, _context), do: {:atomic, %{}}
  end

  defmodule UnsafeValidation do
    @moduledoc false
    use Ash.Resource.Validation

    def supports(_opts), do: [Ash.Changeset, Ash.Query, Ash.ActionInput]
    def describe(_opts), do: [message: "test validation", vars: []]
    def validate(_subject, _opts, _context), do: :ok
  end

  defmodule SafeValidation do
    @moduledoc false
    use Ash.Resource.Validation

    def supports(_opts), do: [Ash.Changeset, Ash.Query, Ash.ActionInput]
    def describe(_opts), do: [message: "test validation", vars: []]
    def temporal_safe?(_opts), do: true
    def validate(_subject, _opts, _context), do: :ok
  end

  defmodule UnsafePreparation do
    @moduledoc false
    use Ash.Resource.Preparation

    def supports(_opts), do: [Ash.Query, Ash.ActionInput]
    def prepare(subject, _opts, _context), do: subject
  end

  defmodule SafePreparation do
    @moduledoc false
    use Ash.Resource.Preparation

    def supports(_opts), do: [Ash.Query, Ash.ActionInput]
    def temporal_safe?(_opts), do: true
    def prepare(subject, _opts, _context), do: subject
  end

  # Implements the behaviour by hand, without `use`, so it has no `temporal_safe?/1` at all.
  defmodule BareChange do
    @moduledoc false
    @behaviour Ash.Resource.Change

    def init(opts), do: {:ok, opts}
    def change(changeset, _opts, _context), do: changeset
    def batch_callbacks?(_, _, _), do: false
    def atomic?, do: false
    def has_change?, do: true
    def has_batch_change?, do: false
    def has_after_batch?, do: false
    def has_before_batch?, do: false
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain, validate_config_inclusion?: false

    resources do
      allow_unregistered? true
    end
  end

  defmodule Versioned do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    temporal do
      strategy :context
      attribute :valid_at
    end

    actions do
      defaults [:read, :destroy]

      create :create do
        primary? true
        accept [:id, :name]
      end

      create :create_unsafe_change do
        accept [:id, :name]
        change UnsafeChange
      end

      create :create_configured_safe_change do
        accept [:id, :name]
        change ConfiguredSafeChange
      end

      create :create_safe_change do
        accept [:id, :name]
        change SafeChange
      end

      create :create_opts_safe do
        accept [:id, :name]
        change {OptsDependentChange, safe?: true}
      end

      create :create_opts_unsafe do
        accept [:id, :name]
        change {OptsDependentChange, safe?: false}
      end

      create :create_anonymous_change do
        accept [:id, :name]
        change fn changeset, _context -> changeset end
      end

      create :create_before_action do
        accept [:id, :name]
        change before_action(fn changeset -> changeset end)
      end

      create :create_unsafe_validation do
        accept [:id, :name]
        validate UnsafeValidation
      end

      create :create_safe_validation do
        accept [:id, :name]
        validate SafeValidation
      end

      create :create_unsafe_where do
        accept [:id, :name]
        change SafeChange, where: [UnsafeValidation]
      end

      create :create_anonymous_validation do
        accept [:id, :name]
        validate fn _changeset, _context -> :ok end
      end

      create :create_with_builtins do
        accept [:id, :name]
        change set_attribute(:name, "builtin")
        change set_attribute(:stamped_at, &DateTime.utc_now/0)
        validate present(:id)
        validate string_length(:name, min: 1)
        validate one_of(:name, ["builtin"])
        validate negate(absent(:name))
        validate all([present(:name), compare(:id, greater_than: 0)])
        validate any([absent(:name), present(:name)])
      end

      create :create_negated_unsafe do
        accept [:id, :name]
        validate negate(UnsafeValidation)
      end

      create :create_all_unsafe do
        accept [:id, :name]
        validate all([present(:name), UnsafeValidation])
      end

      update :update do
        primary? true
        accept [:name]
      end

      update :update_unsafe_atomic do
        accept [:name]
        change UnsafeAtomicChange
      end

      update :update_safe_atomic do
        accept [:name]
        change SafeAtomicChange
      end

      read :read_unsafe_preparation do
        prepare UnsafePreparation
      end

      read :read_safe_preparation do
        prepare SafePreparation
      end

      read :read_anonymous_preparation do
        prepare fn query, _context -> query end
      end

      read :read_before_action do
        prepare before_action(fn query -> query end)
      end

      read :read_with_builtins do
        prepare build(sort: [:id])
        validate SafeValidation
      end

      read :read_unsafe_validation do
        validate UnsafeValidation
      end

      action :generic_unsafe_preparation, :atom do
        prepare UnsafePreparation
        run fn _input, _context -> {:ok, :ran} end
      end

      action :generic_safe_preparation, :atom do
        prepare SafePreparation
        validate SafeValidation
        run fn _input, _context -> {:ok, :ran} end
      end

      action :generic_unsafe_validation, :atom do
        validate UnsafeValidation
        run fn _input, _context -> {:ok, :ran} end
      end
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :name, :string, public?: true
      attribute :stamped_at, :utc_datetime_usec, public?: true
    end
  end

  defmodule Plain do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read, :destroy]

      create :create do
        primary? true
        accept [:id, :name]
        change UnsafeChange
        change fn changeset, _context -> changeset end
        validate UnsafeValidation
      end

      read :read_unsafe do
        prepare UnsafePreparation
        prepare fn query, _context -> query end
      end
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :name, :string, public?: true
    end
  end

  defp assert_not_temporal_safe(fun, module, type) do
    error = assert_raise(Ash.Error.Framework, fun)

    assert [%NotTemporalSafe{module: ^module, type: ^type, resource: Versioned} = inner] =
             error.errors

    assert Exception.message(inner) =~ "#{inspect(module)} is not temporal safe"
    assert Exception.message(inner) =~ "def temporal_safe?(_opts), do: true"
  end

  describe "Ash.Temporal.temporal_safe?/2" do
    test "is false for a module that has not declared itself safe" do
      refute Ash.Temporal.temporal_safe?(UnsafeChange, [])
      refute Ash.Temporal.temporal_safe?(UnsafeValidation, [])
      refute Ash.Temporal.temporal_safe?(UnsafePreparation, [])
    end

    test "is false for a module that does not define the callback at all" do
      refute Ash.Temporal.temporal_safe?(BareChange, [])
    end

    test "is true for a module that declares itself safe" do
      assert Ash.Temporal.temporal_safe?(SafeChange, [])
      assert Ash.Temporal.temporal_safe?(SafeValidation, [])
      assert Ash.Temporal.temporal_safe?(SafePreparation, [])
    end

    test "is true for a module listed in the `:temporal_safe_modules` config" do
      assert Ash.Temporal.temporal_safe?(ConfiguredSafeChange, [])
    end

    test "consults the options" do
      assert Ash.Temporal.temporal_safe?(OptsDependentChange, safe?: true)
      refute Ash.Temporal.temporal_safe?(OptsDependentChange, safe?: false)
    end

    test "composite validations are safe only when what they compose is" do
      assert Ash.Temporal.temporal_safe?(Ash.Resource.Validation.All,
               validations: [{Ash.Resource.Validation.Present, attributes: [:name]}]
             )

      refute Ash.Temporal.temporal_safe?(Ash.Resource.Validation.All,
               validations: [
                 {Ash.Resource.Validation.Present, attributes: [:name]},
                 UnsafeValidation
               ]
             )

      assert Ash.Temporal.temporal_safe?(Ash.Resource.Validation.Negate,
               validation: {Ash.Resource.Validation.Present, attributes: [:name]}
             )

      refute Ash.Temporal.temporal_safe?(Ash.Resource.Validation.Negate,
               validation: {UnsafeValidation, []}
             )
    end
  end

  describe "changes on a temporal resource" do
    test "a change that has not declared itself temporal safe raises a framework error" do
      assert_not_temporal_safe(
        fn -> Ash.create!(Versioned, %{id: 1, name: "a"}, action: :create_unsafe_change) end,
        UnsafeChange,
        :change
      )
    end

    test "the error names the action" do
      error =
        assert_raise(Ash.Error.Framework, fn ->
          Ash.create!(Versioned, %{id: 1, name: "a"}, action: :create_unsafe_change)
        end)

      assert Exception.message(error) =~ "Versioned.create_unsafe_change"
    end

    test "a change that declared itself temporal safe runs" do
      assert %Versioned{name: "a"} =
               Ash.create!(Versioned, %{id: 1, name: "a"}, action: :create_safe_change)
    end

    test "a change listed as temporal safe in config runs" do
      assert %Versioned{name: "a"} =
               Ash.create!(Versioned, %{id: 1, name: "a"}, action: :create_configured_safe_change)
    end

    test "temporal safety may depend on the change's options" do
      assert %Versioned{} = Ash.create!(Versioned, %{id: 1, name: "a"}, action: :create_opts_safe)

      assert_not_temporal_safe(
        fn -> Ash.create!(Versioned, %{id: 2, name: "a"}, action: :create_opts_unsafe) end,
        OptsDependentChange,
        :change
      )
    end

    test "anonymous function changes are never temporal safe" do
      assert_not_temporal_safe(
        fn -> Ash.create!(Versioned, %{id: 1, name: "a"}, action: :create_anonymous_change) end,
        Ash.Resource.Change.Function,
        :change
      )
    end

    test "hook builtins wrapping a function are never temporal safe" do
      assert_not_temporal_safe(
        fn -> Ash.create!(Versioned, %{id: 1, name: "a"}, action: :create_before_action) end,
        Ash.Resource.Change.BeforeAction,
        :change
      )
    end

    test "the check applies to atomic changes too" do
      record = Ash.create!(Versioned, %{id: 1, name: "a"})

      assert_not_temporal_safe(
        fn ->
          Ash.Changeset.fully_atomic_changeset(Versioned, :update_unsafe_atomic, %{name: "b"})
        end,
        UnsafeAtomicChange,
        :change
      )

      assert %Ash.Changeset{} =
               Ash.Changeset.fully_atomic_changeset(Versioned, :update_safe_atomic, %{name: "b"})

      assert %Versioned{name: "b"} =
               Ash.update!(record, %{name: "b"}, action: :update_safe_atomic)
    end
  end

  describe "validations on a temporal resource" do
    test "a validation that has not declared itself temporal safe raises a framework error" do
      assert_not_temporal_safe(
        fn ->
          Ash.create!(Versioned, %{id: 1, name: "a"}, action: :create_unsafe_validation)
        end,
        UnsafeValidation,
        :validation
      )
    end

    test "a validation used in a `where` is checked too" do
      assert_not_temporal_safe(
        fn -> Ash.create!(Versioned, %{id: 1, name: "a"}, action: :create_unsafe_where) end,
        UnsafeValidation,
        :validation
      )
    end

    test "anonymous function validations are never temporal safe" do
      assert_not_temporal_safe(
        fn ->
          Ash.create!(Versioned, %{id: 1, name: "a"}, action: :create_anonymous_validation)
        end,
        Ash.Resource.Validation.Function,
        :validation
      )
    end

    test "a validation that declared itself temporal safe runs" do
      assert %Versioned{} =
               Ash.create!(Versioned, %{id: 1, name: "a"}, action: :create_safe_validation)
    end

    test "composite validations surface an unsafe member" do
      assert_not_temporal_safe(
        fn -> Ash.create!(Versioned, %{id: 1, name: "a"}, action: :create_negated_unsafe) end,
        Ash.Resource.Validation.Negate,
        :validation
      )

      assert_not_temporal_safe(
        fn -> Ash.create!(Versioned, %{id: 1, name: "a"}, action: :create_all_unsafe) end,
        Ash.Resource.Validation.All,
        :validation
      )
    end

    test "read action validations are checked" do
      assert_not_temporal_safe(
        fn -> Ash.read!(Versioned, action: :read_unsafe_validation) end,
        UnsafeValidation,
        :validation
      )
    end
  end

  describe "preparations on a temporal resource" do
    test "a preparation that has not declared itself temporal safe raises a framework error" do
      assert_not_temporal_safe(
        fn -> Ash.read!(Versioned, action: :read_unsafe_preparation) end,
        UnsafePreparation,
        :preparation
      )
    end

    test "a preparation that declared itself temporal safe runs" do
      Ash.create!(Versioned, %{id: 1, name: "a"})
      assert [%Versioned{id: 1}] = Ash.read!(Versioned, action: :read_safe_preparation)
    end

    test "anonymous function preparations are never temporal safe" do
      assert_not_temporal_safe(
        fn -> Ash.read!(Versioned, action: :read_anonymous_preparation) end,
        Ash.Resource.Preparation.Function,
        :preparation
      )
    end

    test "hook builtins wrapping a function are never temporal safe" do
      assert_not_temporal_safe(
        fn -> Ash.read!(Versioned, action: :read_before_action) end,
        Ash.Resource.Preparation.BeforeAction,
        :preparation
      )
    end

    test "generic action preparations and validations are checked" do
      assert_not_temporal_safe(
        fn ->
          Versioned
          |> Ash.ActionInput.for_action(:generic_unsafe_preparation, %{})
          |> Ash.run_action!()
        end,
        UnsafePreparation,
        :preparation
      )

      assert_not_temporal_safe(
        fn ->
          Versioned
          |> Ash.ActionInput.for_action(:generic_unsafe_validation, %{})
          |> Ash.run_action!()
        end,
        UnsafeValidation,
        :validation
      )

      assert :ran =
               Versioned
               |> Ash.ActionInput.for_action(:generic_safe_preparation, %{})
               |> Ash.run_action!()
    end
  end

  describe "builtins" do
    test "the builtin changes, validations and preparations are temporal safe" do
      record =
        Ash.create!(Versioned, %{id: 1, name: "whatever"},
          action: :create_with_builtins,
          as_of: @as_of
        )

      assert record.name == "builtin"
      assert [%Versioned{id: 1}] = Ash.read!(Versioned, action: :read_with_builtins)
    end

    test "set_attribute with `&DateTime.utc_now/0` resolves to the write's as_of" do
      record =
        Ash.create!(Versioned, %{id: 1, name: "whatever"},
          action: :create_with_builtins,
          as_of: @as_of
        )

      assert record.stamped_at == @as_of
    end
  end

  describe "non-temporal resources" do
    test "are not checked" do
      assert %Plain{} = Ash.create!(Plain, %{id: 1, name: "a"})
      assert [%Plain{}] = Ash.read!(Plain, action: :read_unsafe)
    end
  end
end
