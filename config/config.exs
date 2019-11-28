use Mix.Config

if Mix.env() == :test do
  config :ash,
    resources: [
      Ash.Test.Post
    ]
end
