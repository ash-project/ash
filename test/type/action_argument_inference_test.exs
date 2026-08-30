defmodule Ash.Type.ActionArgumentInferenceTest do
  use ExUnit.Case, async: false

  defp compile(source) do
    Code.with_diagnostics(fn -> Code.compile_string(source) end)
  end

  test "an exact arguments map survives an action input struct update" do
    {_modules, diagnostics} =
      compile("""
      defmodule ExactActionArgumentInference do
        def run(%Ash.ActionInput{} = raw_input, context) do
          callback = fn input, _context -> input.arguments.missing end

          input = %Ash.ActionInput{
            raw_input
            | arguments: %{
                amount: raw_input.arguments[:amount],
                reference: raw_input.arguments[:reference]
              }
          }

          callback.(input, context)
        end
      end
      """)

    assert Enum.any?(diagnostics, fn diagnostic ->
             diagnostic.severity == :warning and
               diagnostic.message =~ "incompatible types given on function call" and
               diagnostic.message =~ "arguments: %{amount: term(), reference: term()}" and
               diagnostic.message =~ "missing: term()"
           end)
  end

  test "the current lifted callback boundary does not preserve the action argument shape" do
    {_modules, diagnostics} =
      compile("""
      defmodule LiftedActionArgumentInference do
        def generated_run(input, _context), do: input.arguments.missing

        def invoke(%Ash.ActionInput{} = raw_input, context) do
          input = %Ash.ActionInput{
            raw_input
            | arguments: %{
                amount: raw_input.arguments[:amount],
                reference: raw_input.arguments[:reference]
              }
          }

          __MODULE__.generated_run(input, context)
        end
      end
      """)

    refute Enum.any?(diagnostics, fn diagnostic ->
             diagnostic.message =~ "missing: term()"
           end)
  end

  test "Spark can retain the completed action argument shape in the lifted run body" do
    module = "SparkActionArgumentInference#{System.unique_integer([:positive])}"

    source = """
    defmodule #{module} do
      use Ash.Resource, domain: nil

      actions do
        action :collect do
          argument :amount, :integer, allow_nil?: false
          argument :reference, :string

          run fn input, _context ->
            {:ok, input.arguments.missing}
          end
        end
      end
    end
    """

    {warning, 0} =
      System.cmd(
        System.find_executable("mix"),
        ["run", "--no-compile", "-e", "Code.compile_string(#{inspect(source)})"],
        cd: File.cwd!(),
        env: [{"MIX_ENV", "test"}],
        stderr_to_stdout: true
      )

    assert warning =~ "unknown key .missing"
    assert warning =~ "amount: term()"
    assert warning =~ "reference: term()"
  end

  test "Spark carries the action argument shape into a dispatched change module" do
    suffix = System.unique_integer([:positive])
    change_module = "DispatchedArgumentChange#{suffix}"
    resource_module = "DispatchedArgumentResource#{suffix}"

    source = """
    defmodule #{change_module} do
      use Ash.Resource.Change

      @impl true
      def change(changeset, _opts, _context) do
        _ = changeset.arguments.missing
        changeset
      end
    end

    defmodule #{resource_module} do
      use Ash.Resource, domain: nil

      resource do
        require_primary_key? false
      end

      actions do
        create :create do
          argument :amount, :integer, allow_nil?: false
          argument :reference, :string
          change #{change_module}
        end
      end
    end

    """

    {warning, 0} = compile_dispatched_source(source)

    assert warning =~ "incompatible types given to"

    assert warning =~
             "Ash.TypeWitness.#{change_module}.#{resource_module}.__ash_type_witness__/4"

    assert warning =~ "amount: integer()"
    assert warning =~ "reference: nil or binary()"
    assert warning =~ "missing: term()"
  end

  test "a dispatched change accepting a declared argument does not warn" do
    suffix = System.unique_integer([:positive])
    change_module = "ValidDispatchedArgumentChange#{suffix}"
    resource_module = "ValidDispatchedArgumentResource#{suffix}"

    source = """
    defmodule #{change_module} do
      use Ash.Resource.Change

      @impl true
      def change(changeset, _opts, _context) do
        _ = changeset.arguments.amount
        changeset
      end
    end

    defmodule #{resource_module} do
      use Ash.Resource, domain: nil

      resource do
        require_primary_key? false
      end

      actions do
        create :create do
          argument :amount, :integer, allow_nil?: false
          argument :reference, :string
          change #{change_module}
        end
      end
    end
    """

    {output, 0} = compile_dispatched_source(source)

    refute output =~ "unknown key"
    refute output =~ "incompatible types given"
  end

  test "Spark carries the resource attribute shape into a dispatched change module" do
    suffix = System.unique_integer([:positive])
    change_module = "DispatchedAcceptedAttributeChange#{suffix}"
    resource_module = "DispatchedAcceptedAttributeResource#{suffix}"

    source = """
    defmodule #{change_module} do
      use Ash.Resource.Change

      @impl true
      def change(changeset, _opts, _context) do
        _ = changeset.attributes.missing
        changeset
      end
    end

    defmodule #{resource_module} do
      use Ash.Resource, domain: nil

      resource do
        require_primary_key? false
      end

      attributes do
        attribute :accepted, :string, public?: true
        attribute :not_accepted, :string, public?: true
      end

      actions do
        create :create do
          accept [:accepted]
          change #{change_module}
        end
      end
    end
    """

    {warning, 0} = compile_dispatched_source(source)

    assert warning =~ "incompatible types given to"

    assert warning =~
             "Ash.TypeWitness.#{change_module}.#{resource_module}.__ash_type_witness__/4"

    assert warning =~ "accepted: nil or binary()"
    assert warning =~ "not_accepted: nil or binary()"
    assert warning =~ "missing: term()"
  end

  test "a dispatched change can access a real resource attribute excluded from accept" do
    suffix = System.unique_integer([:positive])
    change_module = "ValidDispatchedAcceptedAttributeChange#{suffix}"
    resource_module = "ValidDispatchedAcceptedAttributeResource#{suffix}"

    source = """
    defmodule #{change_module} do
      use Ash.Resource.Change

      @impl true
      def change(changeset, _opts, _context) do
        _ = changeset.attributes.not_accepted
        changeset
      end
    end

    defmodule #{resource_module} do
      use Ash.Resource, domain: nil

      resource do
        require_primary_key? false
      end

      attributes do
        attribute :accepted, :string, public?: true
        attribute :not_accepted, :string, public?: true
      end

      actions do
        create :create do
          accept [:accepted]
          change #{change_module}
        end
      end
    end
    """

    {output, 0} = compile_dispatched_source(source)

    refute output =~ "unknown key"
    refute output =~ "incompatible types given"
  end

  test "a dispatched change retains embedded resource and embedded array types" do
    suffix = System.unique_integer([:positive])
    embedded_module = "TypedEmbeddedAddress#{suffix}"
    change_module = "TypedEmbeddedArgumentChange#{suffix}"
    resource_module = "TypedEmbeddedArgumentResource#{suffix}"

    source = """
    defmodule #{embedded_module} do
      use Ash.Resource, data_layer: :embedded

      attributes do
        attribute :street, :string, public?: true
        attribute :number, :integer, public?: true
      end
    end

    defmodule #{change_module} do
      use Ash.Resource.Change

      @impl true
      def change(changeset, _opts, _context) do
        _ = changeset.arguments.address.missing
        _ = hd(changeset.arguments.previous_addresses).missing
        changeset
      end
    end

    defmodule #{resource_module} do
      use Ash.Resource, domain: nil

      resource do
        require_primary_key? false
      end

      actions do
        create :create do
          argument :address, #{embedded_module}, allow_nil?: false
          argument :previous_addresses, {:array, #{embedded_module}}, allow_nil?: false
          change #{change_module}
        end
      end
    end
    """

    {warning, 0} = compile_dispatched_source(source)

    assert warning =~ "address: %#{embedded_module}{}"
    assert warning =~ "previous_addresses: list(%#{embedded_module}{})"
    assert warning =~ "missing: term()"
  end

  test "a managed relationship argument retains destination action input types" do
    suffix = System.unique_integer([:positive])
    details_module = "TypedRelationshipDetails#{suffix}"
    child_module = "TypedRelationshipChild#{suffix}"
    change_module = "TypedRelationshipChange#{suffix}"
    parent_module = "TypedRelationshipParent#{suffix}"

    source = """
    defmodule #{details_module} do
      use Ash.Resource, data_layer: :embedded

      attributes do
        attribute :street, :string, public?: true
      end
    end

    defmodule #{child_module} do
      use Ash.Resource, domain: nil

      attributes do
        uuid_primary_key :id
        attribute :parent_id, :uuid, public?: true
        attribute :name, :string, public?: true
        attribute :details, #{details_module}, public?: true
      end

      actions do
        create :create do
          primary? true
          accept [:name, :details]
          argument :note, :string
        end
      end
    end

    defmodule #{change_module} do
      use Ash.Resource.Change

      @impl true
      def change(changeset, _opts, _context) do
        child = hd(changeset.arguments.children)
        _ = child.name
        _ = child.note
        _ = child.details.street
        _ = changeset.arguments.featured_child.missing
        changeset
      end
    end

    defmodule #{parent_module} do
      use Ash.Resource, domain: nil

      attributes do
        uuid_primary_key :id
        attribute :featured_child_id, :uuid, public?: true
      end

      relationships do
        has_many :children, #{child_module}, destination_attribute: :parent_id
        belongs_to :featured_child, #{child_module}, source_attribute: :featured_child_id
      end

      actions do
        create :create do
          argument :children, {:array, :map}, allow_nil?: false
          argument :featured_child, :map, allow_nil?: false
          change manage_relationship(:children, type: :create)
          change manage_relationship(:featured_child, type: :create)
          change #{change_module}
        end
      end
    end

    """

    {warning, 0} = compile_dispatched_source(source)

    assert warning =~
             "Ash.TypeWitness.#{change_module}.#{parent_module}.__ash_type_witness__/4"

    assert warning =~ "children:"
    assert warning =~ "list(%{"
    assert warning =~ "featured_child: %{"
    assert warning =~ "details: %#{details_module}{} or nil"
    assert warning =~ "name: nil or binary()"
    assert warning =~ "note: nil or binary()"
    assert warning =~ "missing: term()"
  end

  test "a specialized change witness checks separate modules through private helpers" do
    suffix = System.unique_integer([:positive])
    child_module = "SpecializedChild#{suffix}"
    change_module = Ash.Test.TypeInference.PrivateChange
    parent_module = "SpecializedParent#{suffix}"

    child_source = """
    defmodule #{child_module} do
      use Ash.Resource, domain: nil

      attributes do
        uuid_primary_key :id
        attribute :parent_id, :uuid, public?: true
        attribute :name, :string, public?: true
      end

      actions do
        create :create do
          primary? true
          accept [:name]
        end
      end
    end
    """

    parent_source = """
    defmodule #{parent_module} do
      use Ash.Resource, domain: nil

      attributes do
        uuid_primary_key :id
      end

      relationships do
        has_many :children, #{child_module}, destination_attribute: :parent_id
      end

      actions do
        create :create do
          argument :children, {:array, :map}, allow_nil?: false
          change manage_relationship(:children, type: :create)
          change #{inspect(change_module)}
        end
      end
    end
    """

    expression = """
    Code.compile_string(#{inspect(child_source)})
    Code.compile_string(#{inspect(parent_source)})
    definition = Ash.Resource.TypeInference.specialized_change_witness_definition(
      #{parent_module},
      #{inspect(change_module)}
    )
    Code.compile_quoted(definition)
    """

    {warning, 0} =
      System.cmd(
        System.find_executable("mix"),
        ["run", "--no-compile", "-e", expression],
        cd: File.cwd!(),
        env: [{"MIX_ENV", "test"}],
        stderr_to_stdout: true
      )

    assert warning =~
             "Ash.TypeWitness.Ash.Test.TypeInference.PrivateChange.#{parent_module}.__ash_type_witness__/4"

    assert warning =~ "children: list(%{"
    assert warning =~ "name: nil or binary()"
    assert warning =~ "missing: term()"
    refute warning =~ "underscored variable"
  end

  defp compile_dispatched_source(source) do
    expression = """
    resources =
      Code.compile_string(#{inspect(source)})
      |> Enum.map(&elem(&1, 0))
      |> Enum.filter(&Ash.Resource.Info.resource?/1)

    resources
    |> Ash.Resource.TypeInference.witness_entries()
    |> Enum.each(&Code.compile_quoted(&1.definition))
    """

    System.cmd(
      System.find_executable("mix"),
      ["run", "--no-compile", "-e", expression],
      cd: File.cwd!(),
      env: [{"MIX_ENV", "test"}],
      stderr_to_stdout: true
    )
  end
end
