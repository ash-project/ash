# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
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
