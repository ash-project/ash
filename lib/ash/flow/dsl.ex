defmodule Ash.Flow.Dsl do
  @debug %Spark.Dsl.Entity{
    name: :debug,
    describe: """
    Declares a step that will inspect its input and provide
    additional debug information.
    """,
    examples: [
      """
      debug :show_some_information do
        input %{post: result(:create_post)}
      end
      """
    ],
    target: Ash.Flow.Step.Debug,
    args: [:name],
    schema: Ash.Flow.Step.Debug.schema()
  }

  @create %Spark.Dsl.Entity{
    name: :create,
    describe: """
    Declares a step that will call a create action on a resource.
    """,
    examples: [
      """
      create :create_post, MyApp.Post, :create
      """
    ],
    no_depend_modules: [:resource, :touches_resources],
    modules: [:manual],
    target: Ash.Flow.Step.Create,
    args: [:name, :resource, :action],
    schema: Ash.Flow.Step.Create.schema()
  }

  @update %Spark.Dsl.Entity{
    name: :update,
    describe: """
    Declares a step that will call a update action on a resource.
    """,
    examples: [
      """
      update :update_post, MyApp.Post, :update do
        record result(:get_post)
      end
      """
    ],
    no_depend_modules: [:resource, :touches_resources],
    modules: [:manual],
    target: Ash.Flow.Step.Update,
    args: [:name, :resource, :action],
    schema: Ash.Flow.Step.Update.schema()
  }

  @validate %Spark.Dsl.Entity{
    name: :validate,
    describe: """
    Validates some input against an action.
    """,
    examples: [
      """
      validate :update_post, MyApp.Post, :update do
        record result(:get_post)
        only_keys [:name]
      end
      """
    ],
    no_depend_modules: [:resource, :touches_resources],
    target: Ash.Flow.Step.Update,
    args: [:name, :resource, :action],
    schema: Ash.Flow.Step.Validate.schema()
  }

  @destroy %Spark.Dsl.Entity{
    name: :destroy,
    describe: """
    Declares a step that will call a destroy action on a resource.
    """,
    examples: [
      """
      destroy :destroy_post, MyApp.Post, :destroy
      """
    ],
    modules: [:manual],
    no_depend_modules: [:resource, :touches_resources],
    target: Ash.Flow.Step.Destroy,
    args: [:name, :resource, :action],
    schema: Ash.Flow.Step.Destroy.schema()
  }

  @read %Spark.Dsl.Entity{
    name: :read,
    describe: """
    Declares a step that will call a read action on a resource.
    """,
    examples: [
      """
      read :destroy_post, MyApp.Post, :destroy
      """
    ],
    no_depend_modules: [:resource, :touches_resources],
    modules: [:manual],
    target: Ash.Flow.Step.Read,
    args: [:name, :resource, :action],
    schema: Ash.Flow.Step.Read.schema()
  }

  @run_flow %Spark.Dsl.Entity{
    name: :run_flow,
    describe: """
    Runs another flow as part of the current flow.
    The return value of the step is the return value of the flow.
    """,
    examples: [
      """
      run_flow :get_org, GetOrgByName do
        input %{
          name: arg(:org_name)
        }
      """
    ],
    no_depend_modules: [:resource, :touches_resources],
    target: Ash.Flow.Step.RunFlow,
    args: [:name, :flow],
    schema: Ash.Flow.Step.RunFlow.schema()
  }

  @custom %Spark.Dsl.Entity{
    name: :custom,
    describe: """
    Runs a custom step module.

    See `Ash.Flow.Step` for the necessary callbacks and more information.
    """,
    examples: [
      """
      custom :do_custom_thing, MyApp.DoCustomThing do
        input %{...}
      end
      """,
      """
      custom :do_custom_thing, {MyApp.DoCustomThing, opt1: :foo, opt2: :bar} do
        input %{...}
      end
      """
    ],
    no_depend_modules: [:custom, :touches_resources],
    target: Ash.Flow.Step.Custom,
    args: [:name, :custom],
    schema: Ash.Flow.Step.Custom.schema()
  }

  @argument %Spark.Dsl.Entity{
    name: :argument,
    describe: """
    An argument to be passed into the flow
    """,
    examples: [
      """
      argument :params, :map do
        default %{}
      end
      """,
      """
      argument :retries, :integer do
        allow_nil? false
      end
      """
    ],
    no_depend_modules: [:type],
    target: Ash.Flow.Argument,
    args: [:name, :type],
    transform: {Ash.Type, :set_type_transformation, []},
    schema: Ash.Flow.Argument.schema()
  }

  @flow %Spark.Dsl.Section{
    name: :flow,
    describe: """
    Details about the flow itself, like description and the successful return type.
    """,
    entities: [
      @argument
    ],
    schema: [
      api: [
        type: {:behaviour, Ash.Api},
        doc: "An api to use by default when calling actions"
      ],
      description: [
        type: :string,
        doc: "A description of the flow"
      ],
      trace_name: [
        type: :string,
        doc: "The name to use when creating traces. Defaults to the short name."
      ],
      short_name: [
        type: :atom,
        doc:
          "A short name to use for the flow. Defaults to the last to parts of the module name, underscored."
      ],
      returns: [
        type: :any,
        doc: """
        The step or step that should constitute the return value.
        """
      ]
    ]
  }

  @transaction %Spark.Dsl.Entity{
    name: :transaction,
    describe: """
    Runs a set of steps in a transaction.
    """,
    schema: Ash.Flow.Step.Transaction.schema(),
    target: Ash.Flow.Step.Transaction,
    args: [:name, :resource],
    recursive_as: :steps,
    entities: [
      steps: []
    ],
    no_depend_modules: [:touches_resources],
    examples: [
      """
      transaction :create_users do
        create :create_user, User, :create do
          input %{
            first_name: {Faker.Person, :first_name, []},
            last_name: {Faker.Person, :last_name, []}
          }
        end

        create :create_user, Org, :create do
          input %{
            first_name: {Faker.Person, :first_name, []},
            last_name: {Faker.Person, :last_name, []}
          }
        end
      end
      """
    ]
  }

  @map %Spark.Dsl.Entity{
    name: :map,
    describe: """
    Runs a set of steps for each item in a provided list.
    """,
    schema: Ash.Flow.Step.Map.schema(),
    target: Ash.Flow.Step.Map,
    args: [:name, :over],
    recursive_as: :steps,
    no_depend_modules: [:touches_resources],
    entities: [
      steps: []
    ],
    examples: [
      """
      map :create_users, range(1, arg(:count)) do
        output :create_user

        create :create_user, Org, :create do
          input %{
            first_name: {Faker.Person, :first_name, []},
            last_name: {Faker.Person, :last_name, []}
          }
        end
      end
      """
    ]
  }

  @branch %Spark.Dsl.Entity{
    name: :branch,
    describe: """
    Runs a set of steps based on a given value.
    """,
    schema: Ash.Flow.Step.Branch.schema(),
    target: Ash.Flow.Step.Branch,
    args: [:name, :condition],
    recursive_as: :steps,
    no_depend_modules: [:touches_resources],
    entities: [
      steps: []
    ],
    examples: [
      """
      branch :create_users, result(:create_users?) do
        output :create_user

        create :create_user, Org, :create do
          input %{
            first_name: {Faker.Person, :first_name, []},
            last_name: {Faker.Person, :last_name, []}
          }
        end
      end
      """
    ]
  }

  @steps %Spark.Dsl.Section{
    name: :steps,
    describe: """
    The steps to run.
    """,
    examples: [
      """
      steps do
        # invokes a create action
        create :create_post, MyApp.Post, :create
      end
      """
    ],
    imports: [Ash.Flow.StepHelpers],
    entities: [
      @map,
      @branch,
      @transaction,
      @create,
      @debug,
      @update,
      @destroy,
      @validate,
      @read,
      @run_flow,
      @custom
    ]
  }

  @transformers [
    Ash.Flow.Transformers.SetApi,
    Ash.Flow.Transformers.ValidateUniqueNames,
    Ash.Flow.Transformers.ValidateNoEmptySteps
  ]

  @sections [@flow, @steps]

  @moduledoc """
  The built in flow DSL.
  <!--- ash-hq-hide-start--> <!--- -->

  ## DSL Documentation

  ### Index

  #{Spark.Dsl.Extension.doc_index(@sections)}

  ### Docs

  #{Spark.Dsl.Extension.doc(@sections)}
  <!--- ash-hq-hide-stop--> <!--- -->
  """

  use Spark.Dsl.Extension,
    sections: @sections,
    transformers: @transformers
end
