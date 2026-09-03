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

    assert warning =~ "unknown key .missing"

    assert warning =~
             "Ash.TypeWitness.#{change_module}.#{resource_module}.__ash_type_witness__/4"

    assert warning =~ "amount: integer()"
    assert warning =~ "reference: nil or binary()"
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

  test "a dispatched change retains the Ash changeset struct shape" do
    suffix = System.unique_integer([:positive])
    change_module = "DispatchedChangesetShapeChange#{suffix}"
    resource_module = "DispatchedChangesetShapeResource#{suffix}"

    source = """
    defmodule #{change_module} do
      use Ash.Resource.Change

      @impl true
      def change(changeset, _opts, _context) do
        _ = changeset.not_a_changeset_field
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
          change #{change_module}
        end
      end
    end
    """

    {warning, 0} = compile_dispatched_source(source)

    assert warning =~ "not_a_changeset_field"
    assert warning =~ "%Ash.Changeset{"
  end

  test "a dispatched change retains generated default argument wrappers" do
    suffix = System.unique_integer([:positive])
    change_module = "DispatchedDefaultArgumentChange#{suffix}"
    resource_module = "DispatchedDefaultArgumentResource#{suffix}"

    source = """
    defmodule #{change_module} do
      use Ash.Resource.Change

      @impl true
      def change(changeset, _opts, _context), do: apply_change(changeset)

      defp apply_change(changeset, marker \\\\ nil) do
        _ = marker
        _ = changeset.not_a_changeset_field
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
          change #{change_module}
        end
      end
    end
    """

    {warning, 0} = compile_dispatched_source(source)

    assert warning =~ "not_a_changeset_field"
    refute warning =~ "super must be called"
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

    assert warning =~ "unknown key .missing"

    assert warning =~
             "Ash.TypeWitness.#{change_module}.#{resource_module}.__ash_type_witness__/4"

    assert warning =~ "accepted: nil or binary()"
    assert warning =~ "not_accepted: nil or binary()"
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
    assert warning =~ "unknown key .missing"
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
    assert warning =~ "unknown key .missing"
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

  test "dispatched change options retain their literal keyword shape" do
    suffix = System.unique_integer([:positive])
    change_module = "TypedOptionsChange#{suffix}"
    resource_module = "TypedOptionsResource#{suffix}"

    source = """
    defmodule #{change_module} do
      use Ash.Resource.Change

      def change(changeset, opts, _context) do
        _ = opts.missing
        changeset
      end
    end

    defmodule #{resource_module} do
      use Ash.Resource, domain: nil
      resource do require_primary_key? false end

      actions do
        create :create do
          change {#{change_module}, role: :seller}
        end
      end
    end
    """

    {warning, 0} = compile_dispatched_source(source)
    assert warning =~ "non_empty_list({:role, :seller})"
    assert warning =~ "missing"
  end

  test "the custom compiler renders Ash-aware changeset diagnostics" do
    suffix = System.unique_integer([:positive])
    change_module = "FriendlyDiagnosticChange#{suffix}"
    resource_module = "FriendlyDiagnosticResource#{suffix}"

    Code.compile_string("""
    defmodule #{change_module} do
      use Ash.Resource.Change
      def change(changeset, _opts, _context), do: changeset
    end

    defmodule #{resource_module} do
      use Ash.Resource, domain: nil
      resource do require_primary_key? false end

      attributes do
        attribute :status, :atom, public?: true
      end

      actions do
        create :dispatch do
          accept [:status]
          argument :allocations, {:array, :map}, allow_nil?: false
          change #{change_module}
        end
      end
    end
    """)

    diagnostic = %{
      message: "unknown key .value_test in expression:\n\n    changeset.value_test\n",
      file: "nofile",
      source: "nofile",
      severity: :warning,
      position: 1,
      span: nil,
      details: nil,
      stacktrace: []
    }

    formatted =
      Mix.Tasks.Compile.AshTypeInference.to_mix_diagnostic(diagnostic, %{
        key: {Module.concat([resource_module]), :change, Module.concat([change_module])}
      })

    assert formatted.message =~ "Unknown Ash.Changeset field :value_test"
    assert formatted.message =~ "#{change_module}.change/3"
    assert formatted.message =~ "Action arguments: [:allocations]"
    assert formatted.message =~ "Accepted attributes: [:status]"
  end

  test "an inline CRUD local capture retains its private helper and argument shape" do
    suffix = System.unique_integer([:positive])
    resource_module = Module.concat(["InlineCaptureResource#{suffix}"])

    compiler_options = Code.compiler_options()
    Code.compiler_options(debug_info: true)

    modules =
      Code.compile_string("""
      defmodule #{inspect(resource_module)} do
        use Ash.Resource, domain: nil
        resource do require_primary_key? false end

        actions do
          create :create do
            argument :amount, :integer, allow_nil?: false
            change &apply_change/2
          end
        end

        defp apply_change(changeset, _context) do
          _ = changeset.arguments.missing
          changeset
        end
      end
      """)

    Code.compiler_options(compiler_options)

    directory =
      Path.join(System.tmp_dir!(), "ash-inline-capture-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)

    Enum.each(modules, fn {module, binary} ->
      File.write!(Path.join(directory, "#{module}.beam"), binary)
    end)

    :code.add_patha(String.to_charlist(directory))

    Enum.each(modules, fn {module, _binary} ->
      :code.purge(module)
      :code.delete(module)

      beam = directory |> Path.join("#{module}") |> String.to_charlist()
      {:module, ^module} = :code.load_abs(beam)
    end)

    entries = Ash.Resource.TypeInference.witness_entries([resource_module])
    assert [%{key: {^resource_module, :inline_change}}] = entries

    {_compiled, diagnostics} =
      Code.with_diagnostics(fn ->
        Enum.each(entries, &Code.compile_quoted(&1.definition))
      end)

    assert Enum.any?(diagnostics, fn diagnostic ->
             diagnostic.message =~ "missing" and diagnostic.message =~ "amount: integer()"
           end)
  end

  test "constrained map arguments retain their nested field types" do
    suffix = System.unique_integer([:positive])
    change_module = "ConstrainedMapChange#{suffix}"
    resource_module = "ConstrainedMapResource#{suffix}"

    source = """
    defmodule #{change_module} do
      use Ash.Resource.Change

      def change(changeset, _opts, _context) do
        _ = changeset.arguments.payload.missing
        changeset
      end
    end

    defmodule #{resource_module} do
      use Ash.Resource, domain: nil
      resource do require_primary_key? false end

      actions do
        create :create do
          argument :payload, :map,
            allow_nil?: false,
            constraints: [fields: [name: [type: :string, allow_nil?: false]]]

          change #{change_module}
        end
      end
    end
    """

    {warning, 0} = compile_dispatched_source(source)
    assert warning =~ "payload: %{name: binary()}"
    assert warning =~ "missing"
  end

  test "atomic-only and batch-only change callbacks receive typed changesets" do
    suffix = System.unique_integer([:positive])
    atomic_module = "AtomicOnlyChange#{suffix}"
    batch_module = "BatchOnlyChange#{suffix}"
    resource_module = "AtomicBatchResource#{suffix}"

    source = """
    defmodule #{atomic_module} do
      use Ash.Resource.Change
      def atomic(changeset, _opts, _context), do: changeset.arguments.atomic_missing
    end

    defmodule #{batch_module} do
      use Ash.Resource.Change
      def batch_change([changeset | _], _opts, _context), do: [changeset.arguments.batch_missing]
    end

    defmodule #{resource_module} do
      use Ash.Resource, domain: nil
      resource do require_primary_key? false end

      actions do
        create :create do
          argument :amount, :integer, allow_nil?: false
          change #{atomic_module}
          change #{batch_module}
        end
      end
    end
    """

    {warning, 0} = compile_dispatched_source(source)
    assert warning =~ "atomic_missing"
    assert warning =~ "batch_missing"
    assert warning =~ "amount: integer()"
  end

  test "validation and preparation modules receive typed action arguments" do
    suffix = System.unique_integer([:positive])
    validation_module = "TypedValidation#{suffix}"
    preparation_module = "TypedPreparation#{suffix}"
    resource_module = "ValidationPreparationResource#{suffix}"

    source = """
    defmodule #{validation_module} do
      use Ash.Resource.Validation
      def validate(input, _opts, _context), do: input.arguments.validation_missing
    end

    defmodule #{preparation_module} do
      use Ash.Resource.Preparation
      def prepare(query, _opts, _context), do: query.arguments.preparation_missing
    end

    defmodule #{resource_module} do
      use Ash.Resource, domain: nil
      resource do require_primary_key? false end

      actions do
        create :create do
          argument :amount, :integer, allow_nil?: false
          validate #{validation_module}
        end

        read :read do
          argument :needle, :string, allow_nil?: false
          prepare #{preparation_module}
        end
      end
    end
    """

    {warning, 0} = compile_dispatched_source(source)
    assert warning =~ "validation_missing"
    assert warning =~ "preparation_missing"
    assert warning =~ "amount: integer()"
    assert warning =~ "needle: binary()"
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
