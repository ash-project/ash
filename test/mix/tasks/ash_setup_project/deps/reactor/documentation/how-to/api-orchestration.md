<!--
SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# How to Orchestrate HTTP APIs with Reactor

## Problem
You need to integrate with multiple HTTP APIs in production workflows, handling authentication, rate limits, circuit breakers, API versioning, and service discovery patterns.

## Solution Overview
This guide shows you how to build production-ready API orchestration using `reactor_req`, covering real-world concerns like authentication management, circuit breakers, rate limiting, and service resilience patterns.

## Prerequisites
- Understanding of Reactor basics (inputs, steps, arguments)
- Familiarity with error handling and compensation patterns
- Basic knowledge of HTTP APIs and the `Req` HTTP client

## Setup

Add `reactor_req` to your dependencies:

```elixir
# mix.exs
def deps do
  [
    {:reactor, "~> 0.15"},
    {:reactor_req, "~> 0.1"},
    {:req, "~> 0.5"}
  ]
end
```

## HTTP Client Integration with Reactor

The `reactor_req` package provides direct integration between Reactor and the Req HTTP client. Create `lib/api_client.ex`:

```elixir
defmodule ApiClient do
  use Reactor

  input :base_url
  input :user_id

  req_new :setup_client do
    base_url input(:base_url)
    headers value([{"user-agent", "MyApp/1.0"}])
    retry value(:transient)
    retry_delay value(fn attempt -> 200 * attempt end)
  end

  template :build_profile_url do
    argument :user_id, input(:user_id)
    template "/users/<%= @user_id %>"
  end

  template :build_preferences_url do
    argument :user_id, input(:user_id)
    template "/users/<%= @user_id %>/preferences"
  end

  req_get :fetch_profile do
    request result(:setup_client)
    url result(:build_profile_url)
    headers value(%{"accept" => "application/json"})
  end

  req_get :fetch_preferences do
    request result(:setup_client)
    url result(:build_preferences_url)
  end

  step :combine_data do
    argument :profile, result(:fetch_profile, [:body])
    argument :preferences, result(:fetch_preferences, [:body])

    run fn %{profile: profile, preferences: prefs}, _context ->
      {:ok, %{profile: profile, preferences: prefs}}
    end
  end

  return :combine_data
end
```

## Authentication Management

Handle API authentication and token refresh patterns:

```elixir
defmodule AuthenticatedApiClient do
  use Reactor

  input :client_id
  input :client_secret
  input :api_endpoint

  step :build_oauth_payload do
    argument :client_id, input(:client_id)
    argument :client_secret, input(:client_secret)
    
    run fn %{client_id: client_id, client_secret: client_secret}, _context ->
      payload = %{
        grant_type: "client_credentials",
        client_id: client_id,
        client_secret: client_secret
      }
      {:ok, payload}
    end
  end

  req_post :get_auth_token do
    url value("https://auth.example.com/oauth/token")
    json result(:build_oauth_payload)
  end

  step :extract_access_token do
    argument :token_response, result(:get_auth_token, [:body])
    
    run fn %{token_response: resp}, _context ->
      {:ok, resp["access_token"]}
    end
  end

  template :build_auth_header do
    argument :token, result(:extract_access_token)
    template "Bearer <%= @token %>"
  end

  req_new :prepare_authenticated_client do
    base_url input(:api_endpoint)
    headers [{"authorization", result(:build_auth_header)}]
  end

  req_get :fetch_protected_data do
    request result(:prepare_authenticated_client)
    url value("/protected/data")
  end

  return :fetch_protected_data
end
```

## API Versioning

Handle different API versions by composing version-specific reactors:

```elixir
defmodule UserApiV1 do
  use Reactor

  input :user_id

  req_new :client do
    base_url value("https://api.example.com/v1")
  end

  template :build_user_path do
    argument :user_id, input(:user_id)
    template "/users/<%= @user_id %>"
  end

  req_get :fetch_user do
    request result(:client)
    url result(:build_user_path)
  end

  step :normalize_response do
    argument :response, result(:fetch_user, [:body])
    
    run fn %{response: resp}, _context ->
      normalized = %{
        id: resp["user_id"],
        name: resp["full_name"],
        email: resp["email_address"]
      }
      {:ok, normalized}
    end
  end

  return :normalize_response
end

defmodule UserApiV2 do
  use Reactor

  input :user_id

  req_new :client do
    base_url value("https://api.example.com/v2")
  end

  template :build_user_path do
    argument :user_id, input(:user_id)
    template "/users/<%= @user_id %>"
  end

  req_get :fetch_user do
    request result(:client)
    url result(:build_user_path)
  end

  return :fetch_user
end

defmodule VersionedUserApi do
  use Reactor

  input :api_version, default: "v1"
  input :user_id

  switch :fetch_user_by_version do
    on input(:api_version)

    match "v1" do
      compose :get_user, UserApiV1 do
        argument :user_id, input(:user_id)
      end
      return :get_user
    end

    match "v2" do
      compose :get_user, UserApiV2 do
        argument :user_id, input(:user_id)
      end
      return :get_user
    end

    default do
      flunk :unsupported_version do
        argument :version, input(:api_version)
        message "Unsupported API version: <%= @version %>"
      end
    end
  end

  return :fetch_user_by_version
end
```

## Testing API Integration

Test your API orchestration patterns:

```bash
iex -S mix
```

```elixir
# Test basic HTTP client integration
{:ok, result} = Reactor.run(ApiClient, %{
  base_url: "https://jsonplaceholder.typicode.com", 
  user_id: "1"
})

# Test rate limiting
requests = [%{id: 1}, %{id: 2}, %{id: 3}]
{:ok, results} = Reactor.run(RateLimitedApi, %{requests: requests})

# Test versioning
{:ok, normalized} = Reactor.run(VersionedApi, %{
  api_version: "v2",
  user_id: "123"
})
```

## Related Guides

- [Error Handling Tutorial](../tutorials/02-error-handling.md) - Compensation and undo patterns
- [Async Workflows Tutorial](../tutorials/03-async-workflows.md) - Concurrent processing
- [Performance Optimization](performance-optimization.md) - Scaling and monitoring
