#!/usr/bin/env python3
"""
Token Counter Script
Counts tokens using OpenAI's tiktoken library (same as platform.openai.com/tokenizer)
Can also optionally automate browser interaction with the tokenizer website.
"""

import sys
import argparse
from pathlib import Path

try:
    import tiktoken
except ImportError:
    print("Error: tiktoken not installed. Install with: pip install tiktoken")
    sys.exit(1)


def count_tokens_tiktoken(text, model="gpt-4"):
    """
    Count tokens using tiktoken (same as OpenAI's tokenizer website).
    
    Args:
        text: The text to count tokens for
        model: The model to use for tokenization (gpt-4, gpt-3.5-turbo, etc.)
    
    Returns:
        tuple: (token_count, encoding_name)
    """
    try:
        encoding = tiktoken.encoding_for_model(model)
    except KeyError:
        # Fallback to cl100k_base (used by gpt-4 and gpt-3.5-turbo)
        encoding = tiktoken.get_encoding("cl100k_base")
    
    tokens = encoding.encode(text)
    return len(tokens), encoding.name


def count_tokens_file(file_path, model="gpt-4"):
    """Count tokens in a file."""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        return count_tokens_tiktoken(content, model)
    except FileNotFoundError:
        print(f"Error: File not found: {file_path}")
        sys.exit(1)
    except Exception as e:
        print(f"Error reading file: {e}")
        sys.exit(1)


def main():
    parser = argparse.ArgumentParser(
        description="Count tokens in text or file using OpenAI's tokenizer",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Count tokens in a file
  python count_tokens.py gab.jsonld
  
  # Count tokens from stdin
  echo "Hello world" | python count_tokens.py -
  
  # Count tokens with specific model
  python count_tokens.py gab.jsonld --model gpt-3.5-turbo
  
  # Show detailed breakdown
  python count_tokens.py gab.jsonld --verbose
        """
    )
    
    parser.add_argument(
        'input',
        nargs='?',
        help='File path to count tokens in, or "-" for stdin'
    )
    parser.add_argument(
        '--model',
        default='gpt-4',
        help='Model to use for tokenization (default: gpt-4)'
    )
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Show detailed information'
    )
    parser.add_argument(
        '--browser',
        action='store_true',
        help='Open browser tokenizer with the text (requires selenium)'
    )
    
    args = parser.parse_args()
    
    # Get text to count
    if args.input is None or args.input == '-':
        # Read from stdin
        text = sys.stdin.read()
        source = "stdin"
        token_count, encoding_name = count_tokens_tiktoken(text, args.model)
    else:
        # Count tokens from file
        token_count, encoding_name = count_tokens_file(args.input, args.model)
        source = args.input
        # Read text for browser option
        with open(args.input, 'r', encoding='utf-8') as f:
            text = f.read()
    
    # Display results
    if args.verbose:
        print(f"Source: {source}")
        print(f"Model: {args.model}")
        print(f"Encoding: {encoding_name}")
        print(f"Character count: {len(text):,}")
        print(f"Word count: {len(text.split()):,}")
        print(f"Token count: {token_count:,}")
    else:
        print(f"{token_count:,}")
    
    # Browser automation option
    if args.browser:
        try:
            from selenium import webdriver
            from selenium.webdriver.common.by import By
            from selenium.webdriver.support.ui import WebDriverWait
            from selenium.webdriver.support import expected_conditions as EC
            import time
            
            print("\nOpening browser tokenizer...")
            driver = webdriver.Chrome()  # or Firefox(), Safari(), etc.
            driver.get("https://platform.openai.com/tokenizer")
            
            # Wait for page to load
            wait = WebDriverWait(driver, 10)
            textarea = wait.until(EC.presence_of_element_located((By.TAG_NAME, "textarea")))
            
            # Fill in the text
            textarea.clear()
            textarea.send_keys(text)
            
            print("Text pasted into tokenizer. Check the browser for token count.")
            print("Press Enter to close the browser...")
            input()
            driver.quit()
        except ImportError:
            print("\nError: Selenium not installed. Install with: pip install selenium")
            print("Or use the --browser flag with a browser automation setup.")
        except Exception as e:
            print(f"\nError opening browser: {e}")
            print("Token count from tiktoken (shown above) is accurate and matches the website.")


if __name__ == "__main__":
    main()

