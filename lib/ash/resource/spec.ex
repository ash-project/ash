defmodule Ash.Resource.Spec do
  @moduledoc """
  Defines specification contracts for Ash resources using Spark DSL fragments.

  A spec is a Spark DSL fragment that defines partial resource requirements
  using the same DSL syntax as regular resources. Resources can then include
  these specs as fragments and be verified for compliance.

  ## Usage

  Define a spec as a fragment:

      defmodule MyApp.Specs.Timestamped do
        use Spark.Dsl.Fragment, of: Ash.Resource

        attributes do
          attribute :created_at, :utc_datetime_usec
          attribute :updated_at, :utc_datetime_usec
        end
      end

      defmodule MyApp.Specs.Auditable do
        use Spark.Dsl.Fragment, of: Ash.Resource

        attributes do
          attribute :created_at, :utc_datetime_usec
          attribute :updated_at, :utc_datetime_usec
          attribute :created_by_id, :uuid
        end
      end

  Then implement the specs in a resource:

      defmodule MyApp.Post do
        use Ash.Resource,
          domain: MyApp.Blog,
          specs: [MyApp.Specs.Timestamped, MyApp.Specs.Auditable]

        # Resource must provide the required components from specs
        attributes do
          attribute :created_at, :utc_datetime_usec  # Required by spec
          attribute :updated_at, :utc_datetime_usec  # Required by spec  
          attribute :created_by_id, :uuid            # Required by spec
          # ... other attributes
        end
      end

  The spec verifier will ensure that all required components from the specs
  are present in the implementing resource.

  ## Specs vs Fragments

  - **`specs`** - Fragments that define interface contracts requiring verification
  - **`fragments`** - Regular fragments used for code organization/composition

  You can use both together:

      defmodule MyApp.Post do
        use Ash.Resource,
          domain: MyApp.Blog,
          fragments: [MyApp.Fragments.CommonActions],  # Regular composition
          specs: [MyApp.Specs.Timestamped]             # Contract verification
      end

  ## How it works

  1. Specs are defined as Spark DSL fragments of Ash.Resource
  2. Resources include specs via the `fragments` option (existing Ash feature)
  3. A verifier checks that the resource implements all requirements from its fragments
  4. This leverages existing Ash patterns rather than creating new concepts

  This approach provides:
  - Familiar DSL syntax (same as regular resources)
  - Automatic composition via Spark's fragment system
  - Compile-time verification of spec compliance
  - No new DSL concepts to learn
  """

  @type t :: module

  @doc """
  Returns the attributes defined in a spec fragment.
  """
  @spec attributes(t()) :: [Ash.Resource.Attribute.t()]
  def attributes(spec_fragment) do
    try do
      # Extract attributes from the fragment's DSL config
      case spec_fragment.spark_dsl_config() do
        %{[:attributes] => %{entities: entities}} ->
          entities
          |> Enum.filter(&(&1.__struct__ == Ash.Resource.Attribute))

        _ ->
          []
      end
    rescue
      error ->
        IO.inspect(error, label: "error accessing fragment attributes")
        []
    end
  end

  @doc """
  Returns the relationships defined in a spec fragment.
  """
  @spec relationships(t()) :: [map()]
  def relationships(spec_fragment) do
    try do
      # Extract relationships from the fragment's DSL config
      case spec_fragment.spark_dsl_config() do
        %{[:relationships] => %{entities: entities}} ->
          entities

        _ ->
          []
      end
    rescue
      error ->
        IO.inspect(error, label: "error accessing fragment relationships")
        []
    end
  end

  @doc """
  Returns the actions defined in a spec fragment.
  """
  @spec actions(t()) :: [map()]
  def actions(spec_fragment) do
    try do
      # Extract actions from the fragment's DSL config
      case spec_fragment.spark_dsl_config() do
        %{[:actions] => %{entities: entities}} ->
          entities

        _ ->
          []
      end
    rescue
      error ->
        IO.inspect(error, label: "error accessing fragment actions")
        []
    end
  end

  @doc """
  Returns the calculations defined in a spec fragment.
  """
  @spec calculations(t()) :: [map()]
  def calculations(spec_fragment) do
    try do
      # Extract calculations from the fragment's DSL config
      case spec_fragment.spark_dsl_config() do
        %{[:calculations] => %{entities: entities}} ->
          entities

        _ ->
          []
      end
    rescue
      error ->
        IO.inspect(error, label: "error accessing fragment calculations")
        []
    end
  end

  @doc """
  Returns the aggregates defined in a spec fragment.
  """
  @spec aggregates(t()) :: [map()]
  def aggregates(spec_fragment) do
    try do
      # Extract aggregates from the fragment's DSL config
      case spec_fragment.spark_dsl_config() do
        %{[:aggregates] => %{entities: entities}} ->
          entities

        _ ->
          []
      end
    rescue
      error ->
        IO.inspect(error, label: "error accessing fragment aggregates")
        []
    end
  end
end
