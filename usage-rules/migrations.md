<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>

SPDX-License-Identifier: MIT
-->

# Migrations and Schema Changes

After creating or modifying Ash code, run `mix ash.codegen <short_name_describing_changes>` to ensure any required additional changes are made (like migrations are generated). The name of the migration should be lower_snake_case. In a longer running dev session it's usually better to use `mix ash.codegen --dev` as you go and at the end run the final codegen with a sensible name describing all the changes made in the session.

