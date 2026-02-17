<!--
SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# How to Build Data Processing Pipelines

## Problem
You need to process large datasets through multiple transformation steps with error handling, progress tracking, and efficient batch processing. Your data pipeline should handle millions of records while being resilient to failures.

## Solution Overview
This guide shows you how to build robust ETL (Extract, Transform, Load) and batch processing workflows using Reactor's `map` steps, async processing, and collect patterns. You'll learn to process data efficiently at scale.

## Prerequisites
- Understanding of Reactor basics (inputs, steps, arguments)
- Familiarity with Elixir's `Enum` and `Stream` modules
- Basic knowledge of data processing concepts

## Processing Large Datasets

Reactor uses [Iterex](https://hexdocs.pm/iterex/) internally for efficient, resumable iteration over large datasets. This provides several advantages over standard Elixir streams:

**Key Benefits:**
- **Resumable**: Can pause and resume processing from any point
- **Memory efficient**: Processes data in configurable chunks without loading everything into memory
- **Lazy evaluation**: Only processes data as needed, not all at once
- **Resource management**: Proper cleanup of external resources (files, database connections)

**When you need these patterns:**
- Processing files larger than available memory (> 1GB)
- Long-running ETL jobs that may need to pause/resume
- Data pipelines requiring checkpoint/recovery capabilities
- Streaming data from external APIs with rate limits


## Complete ETL Pipeline Example

Here's a complete data processing pipeline that extracts user data, transforms it, and loads it into multiple destinations:

```elixir
defmodule DataPipeline.UserETL do
  use Reactor

  input :source_file
  input :output_destinations

  # Step 1: Extract - Read and parse data
  step :extract_data, DataPipeline.Steps.ExtractCSV do
    argument :file_path, input(:source_file)
  end

  # Step 2: Validate data quality
  step :validate_data_quality, DataPipeline.Steps.DataQualityCheck do
    argument :raw_data, result(:extract_data)
  end

  # Step 3: Transform users in batches
  map :transform_users do
    source result(:extract_data, [:users])
    allow_async? true
    return :validate_user

    step :clean_user do
      argument :user, element(:transform_users)
      run fn %{user: user} ->
        clean_and_normalize_user(%{user: user, rules: %{}})
      end
    end

    step :enrich_user do
      argument :clean_user, result(:clean_user)
      run &enrich_with_external_data/1
    end

    step :validate_user do
      argument :user, result(:enrich_user)
      run &validate_business_rules/1
    end
  end

  # Step 4: Collect transformation results
  collect :process_results do
    argument :transformed_users, result(:transform_users)
    argument :source_stats, result(:extract_data, [:stats])
    
    transform fn %{transformed_users: users, source_stats: stats} ->
      successful_users = Enum.filter(users, &match?({:ok, _}, &1))
      failed_users = Enum.filter(users, &match?({:error, _}, &1))
      
      %{
        successful: Enum.map(successful_users, &elem(&1, 1)),
        failed: failed_users,
        source_count: stats.total_count,
        success_rate: length(successful_users) / length(users)
      }
    end
  end

  # Step 5: Load to multiple destinations in parallel
  step :load_to_database, DataPipeline.Steps.LoadToDatabase do
    argument :users, result(:process_results, [:successful])
    async? true
  end

  step :load_to_search_index, DataPipeline.Steps.LoadToElasticsearch do
    argument :users, result(:process_results, [:successful])
    async? true
  end

  step :generate_report, DataPipeline.Steps.GenerateProcessingReport do
    argument :results, result(:process_results)
    wait_for [:load_to_database, :load_to_search_index]
  end

  return :generate_report
end
```

## Step Implementations

### 1. Data Extraction

```elixir
defmodule DataPipeline.Steps.ExtractCSV do
  use Reactor.Step

  @impl true
  def run(%{file_path: path}, _context, _options) do
    case File.exists?(path) do
      true ->
        users = 
          path
          |> File.stream!()
          |> CSV.decode!(headers: true)
          |> Enum.to_list()
        
        stats = %{
          total_count: length(users),
          file_size: File.stat!(path).size,
          extracted_at: DateTime.utc_now()
        }
        
        {:ok, %{users: users, stats: stats}}
        
      false ->
        {:error, "Source file not found: #{path}"}
    end
  end
end
```

### 2. Data Quality Validation

```elixir
defmodule DataPipeline.Steps.DataQualityCheck do
  use Reactor.Step

  @impl true
  def run(%{raw_data: %{users: users}}, _context, _options) do
    # Analyze data quality
    quality_issues = analyze_quality(users)
    
    rules = %{
      email_required: true,
      phone_format: ~r/^\+?[\d\s\-\(\)]+$/,
      name_min_length: 2,
      max_age: 120
    }
    
    case quality_issues do
      [] -> 
        {:ok, %{rules: rules, issues: [], status: :passed}}
      issues when length(issues) < 100 ->
        {:ok, %{rules: rules, issues: issues, status: :warnings}}
      issues ->
        {:error, "Too many quality issues: #{length(issues)} problems found"}
    end
  end

  defp analyze_quality(users) do
    users
    |> Enum.with_index()
    |> Enum.flat_map(fn {user, index} ->
      check_user_quality(user, index)
    end)
  end

  defp check_user_quality(user, index) do
    issues = []
    
    issues = if is_nil(user["email"]) or user["email"] == "" do
      ["Row #{index + 1}: Missing email" | issues]
    else
      issues
    end
    
    issues = if is_nil(user["name"]) or String.length(user["name"]) < 2 do
      ["Row #{index + 1}: Invalid name" | issues]
    else
      issues
    end
    
    issues
  end
end
```

### 3. User Transformation Functions

```elixir
def clean_and_normalize_user(%{user: user, rules: _rules}) do
  cleaned = %{
    id: user["id"],
    email: String.downcase(String.trim(user["email"] || "")),
    name: String.trim(user["name"] || ""),
    phone: normalize_phone(user["phone"]),
    age: parse_age(user["age"]),
    created_at: DateTime.utc_now()
  }
  
  {:ok, cleaned}
rescue
  e -> {:error, "Failed to clean user #{user["id"]}: #{inspect(e)}"}
end

def enrich_with_external_data(%{clean_user: user}) do
  case ExternalAPI.get_user_profile(user.email) do
    {:ok, profile} ->
      enriched = Map.merge(user, %{
        company: profile.company,
        location: profile.location,
        verified: profile.verified
      })
      {:ok, enriched}
      
    {:error, :not_found} ->
      # Continue without enrichment
      {:ok, Map.put(user, :verified, false)}
      
    {:error, reason} ->
      {:error, "Enrichment failed for #{user.email}: #{reason}"}
  end
end

def validate_business_rules(%{user: user}) do
  errors = []
  
  errors = if String.length(user.name) < 2 do
    ["Name too short" | errors]
  else
    errors
  end
  
  errors = if not String.contains?(user.email, "@") do
    ["Invalid email format" | errors]
  else
    errors
  end
  
  errors = if user.age && user.age > 120 do
    ["Unrealistic age" | errors]
  else
    errors
  end
  
  case errors do
    [] -> {:ok, user}
    errors -> {:error, "Validation failed: #{Enum.join(errors, ", ")}"}
  end
end
```

### 4. Data Loading Steps

```elixir
defmodule DataPipeline.Steps.LoadToDatabase do
  use Reactor.Step

  @impl true
  def run(%{users: users}, _context, _options) do
    batches = Enum.chunk_every(users, 1000)
    
    results = Enum.map(batches, fn batch ->
      case MyApp.Repo.insert_all("users", batch, 
           on_conflict: :replace_all,
           conflict_target: [:email]) do
        {count, _} -> {:ok, count}
        error -> {:error, error}
      end
    end)
    
    total_inserted = results
    |> Enum.filter(&match?({:ok, _}, &1))
    |> Enum.map(&elem(&1, 1))
    |> Enum.sum()
    
    {:ok, %{inserted: total_inserted, total_batches: length(batches)}}
  end

  @impl true
  def compensate(_reason, %{users: users}, _context, _options) do
    # Cleanup on failure - remove any partially inserted data
    user_emails = Enum.map(users, & &1.email)
    MyApp.Repo.delete_all(from u in "users", where: u.email in ^user_emails)
    :ok
  end
end

defmodule DataPipeline.Steps.LoadToElasticsearch do
  use Reactor.Step

  @impl true
  def run(%{users: users}, _context, _options) do
    bulk_requests = Enum.map(users, fn user ->
      %{
        index: %{
          _index: "users",
          _id: user.id,
          _source: user
        }
      }
    end)
    
    case Elasticsearch.bulk_request(bulk_requests) do
      {:ok, response} ->
        indexed = response["items"] |> length()
        {:ok, %{indexed: indexed}}
        
      {:error, reason} ->
        {:error, "Elasticsearch indexing failed: #{reason}"}
    end
  end
end
```

## Running the Pipeline

```elixir
# Process a large CSV file
{:ok, report} = Reactor.run(DataPipeline.UserETL, %{
  source_file: "/data/users_export.csv",
  output_destinations: [:database, :elasticsearch]
})

IO.puts("Processing completed!")
IO.puts("Success rate: #{report.success_rate * 100}%")
IO.puts("Total processed: #{report.source_count}")
```

## Advanced Patterns

### Streaming Large Files

For very large files, process data in chunks to avoid memory issues and enable efficient error handling:

```elixir
step :extract_streaming_data do
  run fn %{file_path: path} ->
    iter = 
      path
      |> File.stream!()
      |> CSV.decode!(headers: true)
      |> Iter.from()
    
    {:ok, iter}
  end
end

map :process_streaming_data do
  source result(:extract_streaming_data)
  allow_async? true
  return :process_chunk

  compose :process_chunk, ChunkProcessor do
    argument :chunk_data, element(:process_streaming_data)
  end
end

defmodule ChunkProcessor do
  use Reactor

  input :chunk_data

  step :validate_chunk do
    argument :data, input(:chunk_data)
    run &validate_chunk_data/1
  end

  step :transform_records do
    argument :data, result(:validate_chunk)
    run fn %{data: records} ->
      processed = Enum.map(records, &transform_record/1)
      {:ok, processed}
    end
  end

  step :save_checkpoint do
    argument :original, input(:chunk_data)
    argument :processed, result(:transform_records)
    
    run fn %{original: orig, processed: proc} ->
      save_chunk_checkpoint(orig, proc)
      {:ok, proc}
    end
    
    compensate fn _reason, %{original: chunk_data}, _context, _options ->
      log_failed_chunk(chunk_data)
      :retry
    end
  end

  return :save_checkpoint
end
```

### Resumable Data Processing

Pause and resume processing across reactor executions:

```elixir
step :prepare_processing_iter do
  argument :source_iter, result(:extract_streaming_data)
  
  run fn %{source_iter: iter} ->
    if File.exists?("/tmp/processing_checkpoint.json") do
      # Resume from checkpoint
      checkpoint = load_checkpoint()
      remaining_iter = Iter.drop(iter, checkpoint.processed_count)
      {:ok, remaining_iter}
    else
      # Start fresh processing
      {:ok, iter}
    end
  end
end
```

### Parallel Processing with Error Handling

```elixir
map :process_files do
  source input(:file_list)
  allow_async? true

  step :process_file do
    argument :file_path, element(:process_files)
    max_retries 3
    
    run fn %{file_path: path} ->
      case process_single_file(path) do
        {:ok, result} -> {:ok, result}
        {:error, :temp_failure} -> :retry
        {:error, reason} -> {:error, reason}
      end
    end
    
    compensate fn reason, %{file_path: path}, _, _ ->
      case reason do
        %File.Error{} -> :retry
        _other -> :ok
      end
    end
  end
end
```

### Data Aggregation

```elixir
map :calculate_totals do
  source input(:sales_data)
  
  step :sum_by_region do
    argument :region_data, element(:calculate_totals)
    
    run fn %{region_data: data} ->
      total = Enum.sum(Enum.map(data, & &1.amount))
      {:ok, %{region: data.region, total: total}}
    end
  end
end

collect :overall_totals do
  argument :region_totals, result(:calculate_totals)
  
  transform fn %{region_totals: totals} ->
    grand_total = Enum.sum(Enum.map(totals, & &1.total))
    %{grand_total: grand_total, by_region: totals}
  end
end
```


## Monitoring and Observability

### Progress Tracking

```elixir
debug :log_progress do
  argument :batch_results, result(:transform_users)
  argument :message, value("Batch processing completed")
end

# Or use a regular step for custom progress tracking
step :track_progress do
  argument :batch_results, result(:transform_users)
  
  run fn %{batch_results: results} ->
    completed = length(results)
    successful = Enum.count(results, &match?({:ok, _}, &1))
    
    IO.puts("Processed #{completed} records, #{successful} successful")
    {:ok, %{completed: completed, successful: successful}}
  end
end
```

### Telemetry Integration

Reactor provides observability using the conventional [telemetry](https://hex.pm/packages/telemetry) package. Add the telemetry middleware to emit events for monitoring your data pipelines:

```elixir
defmodule DataPipeline.UserETL do
  use Reactor
  
  middlewares do
    middleware Reactor.Middleware.Telemetry
  end
  
  # Steps...
end

# In your application
:telemetry.attach("data-pipeline-handler", 
  [:reactor, :step, :stop], 
  &DataPipeline.Telemetry.handle_event/4, 
  %{}
)
```


## Troubleshooting

### Common Issues

**Data inconsistency:**
- Implement proper compensation for database operations
- Use database transactions where appropriate
- Add data validation at multiple stages

For memory and performance issues, see [Performance Optimization](performance-optimization.md#troubleshooting).

### Debugging Tips

Add debug steps to monitor data flow:

```elixir
debug :inspect_batch do
  argument :batch, element(:transform_users)
  argument :label, value("Processing batch")
end
```


## Related Guides

- [Performance Optimization](performance-optimization.md) - Scaling techniques
- [Testing Strategies](testing-strategies.md) - Testing data pipelines  
- [Async Workflows Tutorial](../tutorials/03-async-workflows.md) - Concurrency basics
