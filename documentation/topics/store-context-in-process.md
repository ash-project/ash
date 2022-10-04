# Store Context In Process

There are various things that can be stored in the process dictionary as opposed to passing them to every function. This is a stylistic choice, and in many cases could lead to *less* clear code, so use with caution. See the functions in the `Ash` module for more.

The following things can be stored in the process:

- Query/changeset context, will be merged with the context of any query/changeset before it is run.
- The current actor (i.e current_user)
- Whether or not to run authorization, i.e `authorize?`
- The current tracer 
- The current tenant
