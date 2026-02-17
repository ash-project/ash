<!--
SPDX-FileCopyrightText: 2020 Zach Daniel
SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Upgrading to 2.0

A 2.0 release was published with a minor breaking change. We decided to vendor `NimbleOptions` (copy their code into our codebase) so that we could make some necessary modifications to it. What this means for users is primarily that:

1. we no longer depend on `NimbleOptions`
2. if you are matching on `NimbleOptions.ValidationError` you will need to update your code to match on `Spark.Options.ValidationError`
