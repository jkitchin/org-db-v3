#!/usr/bin/env python3
"""Test what Emacs would send when parsing tasks.org."""

import json
import sys
from pathlib import Path

def simulate_emacs_parse():
    """Simulate what the Emacs parser would send."""

    tasks_org = Path(__file__).parent.parent / "tasks.org"

    print(f"Reading: {tasks_org}")
    print()

    # This simulates what org-db-v3-parse-buffer-to-json would create
    # We'll build a minimal version to see what linked_files should look like

    # Read the content
    with open(tasks_org) as f:
        content = f.read()

    # Find the line with tasks.pdf
    pdf_line = None
    for i, line in enumerate(content.split('\n'), 1):
        if 'tasks.pdf' in line:
            pdf_line = i
            print(f"Found PDF link on line {pdf_line}: {line.strip()}")
            break

    if not pdf_line:
        print("ERROR: No tasks.pdf link found!")
        return False

    # Show what the linked_files array should contain
    tasks_pdf = tasks_org.parent / "tasks.pdf"

    print()
    print("Expected linked_files array in JSON:")
    print("-" * 70)

    linked_files = [
        {
            "file_path": str(tasks_pdf),
            "org_link_line": pdf_line
        }
    ]

    print(json.dumps(linked_files, indent=2))
    print("-" * 70)
    print()

    # Check the configuration values
    print("Emacs configuration to check:")
    print("-" * 70)
    print("In Emacs, evaluate these:")
    print()
    print("  (message \"org-db-v3-index-linked-files: %s\" org-db-v3-index-linked-files)")
    print()
    print("  (with-current-buffer (find-file-noselect \"tasks.org\")")
    print("    (let* ((parse-tree (org-element-parse-buffer))")
    print("           (linked-files (org-db-v3-parse-linked-files parse-tree)))")
    print("      (message \"Found %d linked files\" (length linked-files))")
    print("      linked-files))")
    print()
    print("-" * 70)
    print()

    # Check server logs for indexing attempts
    log_file = Path("/tmp/org-db-server.log")
    if log_file.exists():
        print("Checking server logs for PDF processing:")
        print("-" * 70)
        with open(log_file) as f:
            logs = f.readlines()

        relevant = [line for line in logs if 'tasks.pdf' in line or 'linked file' in line.lower()]
        if relevant:
            for line in relevant[-10:]:  # Last 10 relevant lines
                print(line.rstrip())
        else:
            print("No mentions of tasks.pdf or linked files in logs")
        print("-" * 70)
        print()

    return True


if __name__ == "__main__":
    print("=" * 70)
    print("Testing Emacs parser behavior for tasks.org")
    print("=" * 70)
    print()

    simulate_emacs_parse()

    print()
    print("=" * 70)
    print("Next steps:")
    print("=" * 70)
    print("1. Make sure you've reloaded the elisp files:")
    print("   (load-file \"elisp/org-db-v3-parse.el\")")
    print("   (load-file \"elisp/org-db-v3-client.el\")")
    print()
    print("2. Check that linked file indexing is enabled:")
    print("   org-db-v3-index-linked-files should be t")
    print()
    print("3. Index tasks.org:")
    print("   M-x org-db-v3-index-file")
    print("   Or: H-v u (from tasks.org buffer)")
    print()
    print("4. Watch the minibuffer message for linked file count")
    print("=" * 70)
