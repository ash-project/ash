query =
  AshExample.Api.query(AshExample.Representative)
  |> Ash.Query.filter(
    id: [in: ["35c73627-d24c-4d37-99c5-f6309202f514", "35c73627-d24c-4d37-99c5-f6309202f514"]]
  )

candidate =
  AshExample.Api.query(AshExample.Representative)
  |> Ash.Query.filter(id: "35c73627-d24c-4d37-99c5-f6309202f514")

Benchee.run(
  %{
    "picosat" => fn ->
      Ash.SatSolver.strict_filter_subset(query.filter, candidate.filter)
    end
  },
  time: 10,
  memory_time: 2
)
