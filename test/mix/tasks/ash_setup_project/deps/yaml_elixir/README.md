# YAML Parser for Elixir

[![Build Status](https://travis-ci.org/KamilLelonek/yaml-elixir.svg)](https://travis-ci.org/KamilLelonek/yaml-elixir)
[![Module Version](https://img.shields.io/hexpm/v/yaml_elixir.svg)](https://hex.pm/packages/yaml_elixir)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/yaml_elixir/)
[![Total Download](https://img.shields.io/hexpm/dt/yaml_elixir.svg)](https://hex.pm/packages/yaml_elixir)
[![License](https://img.shields.io/hexpm/l/yaml_elixir.svg)](https://github.com/KamilLelonek/yaml-elixir/blob/master/LICENSE)
[![Last Updated](https://img.shields.io/github/last-commit/KamilLelonek/yaml-elixir.svg)](https://github.com/KamilLelonek/yaml-elixir/commits/master)

This is a wrapper for [yamerl](https://github.com/yakaz/yamerl) - a native Erlang `YAML` parser which brings all of the functionalities to Elixir language.

## Installation

Add `:yaml_elixir` as a dependency in your `mix.exs` file.

```elixir
defp deps do
  [
     # ...
    {:yaml_elixir, "~> x.x"},
  ]
end
```

Where `x.x.x` equals the version in [`mix.exs`](mix.exs) (you can omit the last `x`). **Always make sure to use the latest version**.

Once you've done that, run `mix deps.get` in your command line to fetch the dependency.

## Usage

With `YamlElixir` you have an access to two functionalities: one for parsing a string and an another one for parsing a file.

Run `iex -S mix` in your terminal to try how their works.

### Parsing a string

```elixir
yaml = """
  a: a
  b: 1
  c: true
  d: ~
  e: nil
"""
"  a: a\n  b: 1\n  c: true\n  d: ~\n  e: nil\n"
YamlElixir.read_from_string(yaml)
{:ok, %{"a" => "a", "b" => 1, "c" => true, "d" => nil, "e" => "nil"}}
```

### Parsing a file

```elixir
path = Path.join(File.cwd!(), "test/fixtures/flat.yml")
"/Users/KamilLelonek/Development/yaml-elixir/test/fixtures/flat.yml"
YamlElixir.read_from_file(path)
{:ok, %{"a" => "a", "b" => 1, "c" => true, "d" => nil, "e" => []}}
```

### Support for atoms

By default, all map keys are processed as strings, as are all bareword or quoted
values.

If you prefer to autodetect keys and values that begin with `:` as atoms, this can be accomplished by passing `atoms: true` as an option to any of the `read_*` functions.

```elixir
yaml = """
  a: a
  b: 1
  c: true
  d: ~
  e: nil
  :f: :atom
"""
"  a: a\n  b: 1\n  c: true\n  d: ~\n  e: nil\n"
YamlElixir.read_from_string(yaml, atoms: true)
{:ok, %{:f => :atom, "a" => "a", "b" => 1, "c" => true, "d" => nil, "e" => "nil"}}
```

Atoms are not garbage collected by `BEAM`, so be careful with this option, and
don't use it with user-supplied input.

If you enable autodetection of atoms, any string values entered (e.g. `":not_really_an_atom"`) will be converted to atoms, as well. If you only need to support a few atom values, it _might_ be better to enable `yamerl's` custom tag for atoms:

```elixir
:yamerl_app.set_param(:node_mods, [:yamerl_node_erlang_atom])
```

and then using the somewhat inconvenient syntax for it:

```yaml
atom_key: !<tag:yamerl,2012:atom> atom_value
```

### Support for keyword lists

Keyword lists can be returned in two ways. Either all maps can be transformed into keyword
lists via the option `maps_as_keywords: true` or individually with a tag. To mark a block
as a keyword list you must first pass in the node module which can process the tokens:

```elixir
:yamerl_app.set_param(:node_mods, [YamlElixir.Node.KeywordList])
```

and then tag the desired block:

```yaml
prod:
  foo: !<tag:yaml_elixir,2019:keyword_list>
    foo: bar
    bar: foo
```

This will return:

```elixir
%{"prod" => %{"foo" => [{"bar", "foo"}, {"foo", "bar"}]}}
```

Note that due to a quirk in how `yamerl` parses YAML documents, using the flow style with
this tag will not work. Do not expect your document to be processed if you write your
YAML like this:

```yaml
prod:
  foo: !<tag:yaml_elixir,2019:keyword_list> {foo: bar, bar: foo}
```

### Elixir Sigil

The `YamlElixir.Sigil` module provides the `~y` sigil that can be useful for example for keeping short configurations or other inlined YAML.

```elixir
import YamlElixir.Sigil

@config ~y"""
debug: false
port: 9200
files:
  - some/file.csv
  - another/file.csv
"""
```

Use the `a` sigil modifier to turn on atom values from YAML:

```elixir
~y":answer: yes"a
```

You can find more examples in [`test` directory](https://github.com/KamilLelonek/yaml-elixir/blob/master/test/yaml_elixir_test.exs).

### Merging anchors

In case your YAML contains [anchors](http://blogs.perl.org/users/tinita/2019/05/reusing-data-with-yaml-anchors-aliases-and-merge-keys.html), you can have these resolved by passing `merge_anchors: true`:

```elixir
yaml = """
  foo: &foo
    bar: 42
  baz:
    <<: *foo
"""
"  foo: &foo\n    bar: 42\n  baz:\n    <<: *foo\n"
YamlElixir.read_from_string(yaml, merge_anchors: true)
```

will result in

```elixir
%{"yaml" => %{"foo" => %{"bar" => 42}, "baz" => %{"bar" => 42}}}
```

## Mix tasks

Sometimes, you may want to use `yaml_elixir` in your `mix` tasks. To do that, you must ensure that the application has started.

    Application.ensure_all_started(:yaml_elixir)

After that, you will be able to use `:yaml-elixir` in your `mix` tasks.

## Contribution

In case of any problems or suggestions do not hesitate and create a pull request.

### Credits

- [bobbypriambodo](https://github.com/bobbypriambodo)
- [Hajto](https://github.com/Hajto)
- [sambooo](https://github.com/sambooo)
- [ernie](https://github.com/ernie)
- [sibsibsib](https://github.com/sibsibsib)
- [vic](https://github.com/vic)
- [rothsberg](https://github.com/rothsberg)
- [msimonborg](https://github.com/msimonborg)
- [mononym](https://github.com/mononym)
- [EugeneOsadchyi](https://github.com/EugeneOsadchyi)
- [TylerPachal](https://github.com/TylerPachal)
- [kianmeng](https://github.com/kianmeng)
- [floriank](https://github.com/floriank)
- [coryodaniel](https://github.com/coryodaniel)
- [simonmcconnell](https://github.com/simonmcconnell)

## Copyright and License

Copyright (c) 2025 Kamil Lelonek

This library is MIT licensed. See the
[LICENSE](https://github.com/KamilLelonek/yaml-elixir/blob/master/LICENSE.txt) for details.
