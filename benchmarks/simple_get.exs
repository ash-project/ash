Benchee.run(
  %{
    "ecto" => fn -> AshExample.Repo.all(AshExample.Ticket) end,
    "ash" => fn -> AshExample.Api.read!(:ticket) end
  },
  time: 10,
  memory_time: 2
)
