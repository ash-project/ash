Benchee.run(
  %{
    "ecto" => fn ->
      AshExample.Ticket
      |> AshExample.Repo.all()
      |> AshExample.Repo.preload(:representative, in_parallel: false)
    end,
    "ash" => fn ->
      :ticket
      |> AshExample.Api.query()
      |> Ash.Query.side_load(:representative)
      |> AshExample.Api.read!()
    end
  },
  time: 10,
  memory_time: 2
)
