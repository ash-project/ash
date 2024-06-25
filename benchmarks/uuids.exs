Benchee.run(
  %{
    "uuid_v7 raw" => fn ->
      Ash.UUIDv7.bingenerate()
    end,
    "uuid_v7 string" => fn ->
      Ash.UUIDv7.generate()
    end,
    "uuid_v4 string" => fn ->
      Ash.UUID.generate()
    end
})
