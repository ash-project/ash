defmodule Ash.Reactor.Dsl.Inputs do
  @moduledoc """
  The `inputs` entity for the `Ash.Reactor` reactor extension.
  """

  defstruct __identifier__: nil,
            template: %{},
            transform: nil

  @type t :: %__MODULE__{
          __identifier__: any,
          template:
            %{optional(atom) => Reactor.Template.t()}
            | Keyword.t(Reactor.Template.t()),
          transform: nil | (any -> any) | {module, keyword} | mfa
        }

  @doc false
  @spec __entity__ :: Spark.Dsl.Entity.t()
  def __entity__ do
    template_type = Reactor.Template.type()

    input_template_type =
      {:or,
       [
         {:map, :atom, template_type},
         {:keyword_list, [*: [type: template_type]]}
       ]}

    %Spark.Dsl.Entity{
      name: :inputs,
      describe: "Specify the inputs for an action",
      docs: """
      Used to build a map for input into an action.

      You can provide the template value as either a map or keyword list and
      multiple instances of `inputs` will be merged together.
      """,
      examples: [
        """
        inputs %{
          author: result(:get_user),
          title: input(:title),
          body: input(:body)
        }
        """,
        """
        inputs(author: result(:get_user))
        """
      ],
      target: __MODULE__,
      args: [:template],
      identifier: {:auto, :unique_integer},
      imports: [Reactor.Dsl.Argument],
      schema: [
        template: [
          type: input_template_type,
          required: true
        ],
        transform: [
          type:
            {:or,
             [{:spark_function_behaviour, Reactor.Step, {Reactor.Step.TransformAll, 1}}, nil]},
          required: false,
          default: nil,
          doc:
            "An optional transformation function which will transform the inputs before executing the action."
        ]
      ],
      transform: {__MODULE__, :__transform__, []}
    }
  end

  @doc false
  @spec __transform__(t) :: {:ok, t} | {:error, any}
  def __transform__(entity) do
    {:ok, %{entity | template: Map.new(entity.template)}}
  end
end
