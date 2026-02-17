locals_without_parens = [
  all: :*,
  check: 1,
  check: 2,
  property: 1,
  property: 2
]

[
  inputs: [
    ".formatter.exs",
    "mix.exs",
    "lib/**/*.ex",
    "{test,examples}/**/*.exs"
  ],
  locals_without_parens: locals_without_parens,
  export: [locals_without_parens: locals_without_parens]
]
