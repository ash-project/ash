<!--
SPDX-FileCopyrightText: 2020 Zach Daniel
SPDX-FileCopyrightText: 2024 splode contributors <https://github.com/ash-project/splode/graphs.contributors>

SPDX-License-Identifier: MIT
-->
[![Elixir CI](https://github.com/ash-project/splode/actions/workflows/ci.yml/badge.svg)](https://github.com/ash-project/splode/actions/workflows/ci.yml)[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Hex version badge](https://img.shields.io/hexpm/v/splode.svg)](https://hex.pm/packages/splode)
[![Hexdocs badge](https://img.shields.io/badge/docs-hexdocs-purple)](https://hexdocs.pm/splode)
[![REUSE status](https://api.reuse.software/badge/github.com/ash-project/splode)](https://api.reuse.software/info/github.com/ash-project/splode)

# Splode

Splode helps you deal with errors and exceptions in your application that are aggregatable and consistent. The general pattern is that you use the `Splode` module as a top level aggregator of error classes, and whenever you return errors, you return one of your `Splode.Error` structs, or a string, or a keyword list. Then, if you want to group errors together, you can use your `Splode` module to do so. You can also use that module to turn any arbitrary value into a splode error.

See the [documentation on hex](https://hexdocs.pm/splode) for more information
