# Used by "mix format"
locals_without_parens = [
  get: 1,
  index: 1,
  actions: 1,
  defaults: 1,
  attribute: 2,
  attribute: 3,
  belongs_to: 2,
  belongs_to: 3,
  create: 1,
  create: 2,
  update: 1,
  update: 2,
  delete: 1,
  delete: 2,
  has_one: 2,
  has_one: 3,
  has_many: 2,
  has_many: 3,
  many_to_many: 2,
  many_to_many: 3
]

[
  inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"],
  locals_without_parens: locals_without_parens,
  export: [
    locals_without_parens: locals_without_parens
  ]
]
