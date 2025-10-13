# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

[
  tools: [
    {:check_cheat_sheets, command: "mix spark.cheat_sheets --check"},
    {:check_formatter, command: "mix spark.formatter --check"},
    {:doctor, false},
    {:reuse, command: ["pipx", "run", "reuse", "lint", "-q"]}
  ]
]
