# Used by "mix format"
locals_without_parens = [
  get: 1,
  index: 1,
  post: 1,
  attribute: 2,
  belongs_to: 2,
  has_many: 2,
  field: 2
]

[
  inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"],
  locals_without_parens: locals_without_parens,
  export: [
    locals_without_parens: locals_without_parens
  ]
]
