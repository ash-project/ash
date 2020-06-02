---
name: Proposal
about: Suggest an idea for this project
title: ''
labels: enhancement, needs review
assignees: ''

---

**Is your feature request related to a problem? Please describe.**
A clear and concise description of what the problem is. Ex. I'm always frustrated when [...]

**Describe the solution you'd like**
A clear and concise description of what you want to happen.

**Describe alternatives you've considered**
A clear and concise description of any alternative solutions or features you've considered.

**Express the feature either with a change to resource syntax, or with a change to the resource interface**

For example

```elixir
  attributes do
    attribute :foo, :integer, bar: 10 # <- Adding `bar` here would cause <x>
  end
```

Or

```elixir
  Api.read(:resource, bar: 10) # <- Adding `bar` here would cause <x>
```

**Additional context**
Add any other context or screenshots about the feature request here.
