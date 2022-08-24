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
    target: Ash.Flow.Step.Update,
    args: [:name, :resource, :action],
    schema: Ash.Flow.Step.Update.schema()
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
    target: Ash.Flow.Step.Read,
    args: [:name, :resource, :action],
    schema: Ash.Flow.Step.Read.schema()
  }

  @run_flow %Spark.Dsl.Entity{
    name: :run_flow,
    describe: """
    Runs another flow as part of the current flow.
    The return value of the flow is the return value of the step.
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
      returns: [
        type: :any,
        doc: """
        The step or step who's output to return.
        If given a single step, then the result of the step is returned. If given multiple, then a map of step name to result is returned.
        If nothing is provided, then the last step is returned.

        To rename keys in the map of step names to results, use a keyword list, where the key is the step and the value is what should be in
        the returned map.

        For example:

        `returns :step_name`
        `returns [:step_one, :step_two]`
        `returns [step_one: :one, step_two: :two]`
        """
      ]
    ]
  }

  @step_entities [@create, @debug, @update, @destroy, @read, @run_flow, @custom]

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
      steps: @step_entities
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

        update :update_user, User, :update do
          record

        end
        over range(1, arg(:count))
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
      steps: @step_entities
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

  transaction = %{@transaction | entities: [steps: [@map | @step_entities]]}
  map = %{@map | entities: [steps: [@transaction | @step_entities]]}

  @transaction transaction
  @map map

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
    entities: [@map, @transaction] ++ @step_entities
  }

  @transformers [
    Ash.Flow.Transformers.SetApi,
    Ash.Flow.Transformers.ValidateUniqueNames,
    Ash.Flow.Transformers.SetTypes,
    Ash.Flow.Transformers.ValidateNoEmptySteps
  ]

  @sections [@flow, @steps]

  @moduledoc """
  The built in flow DSL.
  <!--- ash-hq-hide-start -->

  ## DSL Documentation

  ### Index

  #{Spark.Dsl.Extension.doc_index(@sections)}

  ### Docs

  #{Spark.Dsl.Extension.doc(@sections)}
  <!--- ash-hq-hide-stop -->
  """

  use Spark.Dsl.Extension,
    sections: @sections,
    transformers: @transformers
end
