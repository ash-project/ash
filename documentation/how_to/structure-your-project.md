# Structure your project

In this guide we'll discuss some best practices for how to structure your project.

## A few notes

* None of the things we show you here are *requirements*, only recommendations.

* We avoid any pattern that requires you to name a file or module in a specific way, or put them in a specific place. This ensures that all connections between one module and another module are explicit rather than implicit.

* We break a common Elixir pattern of having the module name match the file name in one specific way. If the resource has a folder, we suggest putting the `resource.ex` in the folder with the same name. See the example below for more.

```
lib/ # top level lib folder for your whole project
├─ my_app/ # your app's main namespace
│  ├─ accounts/ # The Accounts context
│  │  ├─ user/ # resource w/ additional files
│  │  ├─ token.ex # A resource without additional files
│  │  ├─ password_helper.ex # A non-resource file
│  │  ├─ accounts.ex # The Accounts API module
│  ├─ helpdesk/ # A Helpdesk context
│  │  ├─ notification.ex # A resource without additional files
│  │  ├─ other_file.ex # A non-resource file
│  │  ├─ ticket/ # A resource with additional files
│  │  │  ├─ preparations/ # Components of the reosurce, grouped by type
│  │  │  ├─ changes/
│  │  │  ├─ checks/
│  │  │  ├─ ticket.ex # The resource file
```

Generally speaking, your Ash application lives in the standard place within your elixir application, i.e `lib/my_app`. Within that folder, you create one folder for each context that you have. Each context has an `Ash.Api` module within it, and the resources that live within that context. All resource interaction ultimately goes through an Api module.

Alongside the API module, you have your resources, as well as any other files used in the context. If a resource has any additional files that are used to implement it, they should be placed in a folder with the same name as the resource, in subfolders grouping the files type, and *the resource should be placed there too*. This is optional, as stated above, but we've found that with large contexts it keeps things very simple.
