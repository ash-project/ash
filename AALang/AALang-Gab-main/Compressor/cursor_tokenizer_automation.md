# Automating Token Counting in Cursor

This guide shows how to automate passing text to the OpenAI tokenizer from within Cursor.

## Option 1: Direct Token Counting (Recommended)

Use the `count_tokens.py` script which uses `tiktoken` (OpenAI's official tokenizer) - this gives the same results as the website without needing browser automation.

### Setup:
```bash
pip install tiktoken
```

### Usage:
```bash
# Count tokens in a file
python count_tokens.py gab.jsonld

# Count tokens with verbose output
python count_tokens.py gab.jsonld --verbose

# Count tokens for specific model
python count_tokens.py gab.jsonld --model gpt-3.5-turbo
```

## Option 2: Browser Automation (Clipboard Method)

Use `open_tokenizer.py` to copy text to clipboard and open the tokenizer website.

### Setup:
```bash
pip install pyperclip
```

### Usage:
```bash
# Copy file content to clipboard and open tokenizer
python open_tokenizer.py gab.jsonld

# Or from stdin
echo "Your text here" | python open_tokenizer.py -
```

## Option 3: Using Cursor's Browser Automation

You can ask Cursor's AI assistant to:
1. Read a file
2. Navigate to the tokenizer
3. Fill in the text
4. Get the token count

Example prompt:
```
"Read gab.jsonld, navigate to https://platform.openai.com/tokenizer, paste the content, and tell me the token count"
```

## Option 4: Cursor Command/Shortcut

Create a Cursor command or keyboard shortcut that:
1. Gets the current file or selected text
2. Runs the token counting script
3. Displays the result

You can add this to your Cursor settings or create a custom command.

