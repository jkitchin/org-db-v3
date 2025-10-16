#!/usr/bin/env python3
"""Test that tasks.pdf is found and indexed from tasks.org."""

import json
import sys
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent))

from org_db_server.services.database import Database
from org_db_server.config import settings


def test_tasks_pdf_indexing():
    """Test that tasks.pdf linked in tasks.org is detected."""

    # Read the tasks.org file
    tasks_org = Path(__file__).parent.parent / "tasks.org"

    if not tasks_org.exists():
        print(f"ERROR: tasks.org not found at {tasks_org}")
        return False

    print(f"✓ Found tasks.org at {tasks_org}")

    # Check if tasks.pdf exists
    tasks_pdf = tasks_org.parent / "tasks.pdf"
    if not tasks_pdf.exists():
        print(f"ERROR: tasks.pdf not found at {tasks_pdf}")
        return False

    print(f"✓ Found tasks.pdf at {tasks_pdf}")
    print(f"  File size: {tasks_pdf.stat().st_size / 1024:.1f} KB")

    # Read tasks.org content
    with open(tasks_org) as f:
        content = f.read()

    # Check if link exists in file
    if "tasks.pdf" in content:
        print(f"✓ Found 'tasks.pdf' reference in tasks.org")
        # Find the line number
        for i, line in enumerate(content.split('\n'), 1):
            if "tasks.pdf" in line:
                print(f"  Line {i}: {line.strip()}")
    else:
        print(f"ERROR: 'tasks.pdf' not found in tasks.org content")
        return False

    # Check database for linked files
    db = Database(settings.db_path)
    cursor = db.conn.cursor()

    # Get tasks.org file ID
    cursor.execute("SELECT rowid FROM files WHERE filename = ?", (str(tasks_org),))
    row = cursor.fetchone()

    if not row:
        print(f"\n⚠ tasks.org not yet indexed in database")
        print(f"  Run: M-x org-db-v3-index-file on {tasks_org}")
        return False

    file_id = row[0]
    print(f"\n✓ tasks.org is indexed (file_id={file_id})")

    # Check for linked files
    cursor.execute("""
        SELECT rowid, file_path, file_type, file_size, conversion_status, conversion_error
        FROM linked_files
        WHERE org_file_id = ?
    """, (file_id,))

    linked = cursor.fetchall()

    if not linked:
        print(f"\n⚠ No linked files found in database for tasks.org")
        print(f"  This means tasks.pdf was not indexed")
        print(f"\n  Possible reasons:")
        print(f"  1. org-db-v3-index-linked-files is set to nil")
        print(f"  2. The elisp parser didn't find the link")
        print(f"  3. The link format is not recognized")
        print(f"  4. Indexing failed and was not recorded")
        return False

    print(f"\n✓ Found {len(linked)} linked file(s):")
    for lf in linked:
        lf_id, lf_path, lf_type, lf_size, status, error = lf
        print(f"\n  Linked file ID: {lf_id}")
        print(f"  Path: {lf_path}")
        print(f"  Type: {lf_type}")
        print(f"  Size: {lf_size / 1024:.1f} KB")
        print(f"  Status: {status}")
        if error:
            print(f"  Error: {error}")

        # Check chunks for this linked file
        cursor.execute("""
            SELECT COUNT(*)
            FROM chunks
            WHERE linked_file_id = ?
        """, (lf_id,))
        chunk_count = cursor.fetchone()[0]
        print(f"  Chunks: {chunk_count}")

        if chunk_count > 0:
            # Show a sample chunk
            cursor.execute("""
                SELECT chunk_text
                FROM chunks
                WHERE linked_file_id = ?
                LIMIT 1
            """, (lf_id,))
            sample = cursor.fetchone()[0]
            print(f"  Sample chunk (first 100 chars): {sample[:100]}...")

    return True


if __name__ == "__main__":
    print("=" * 70)
    print("Testing tasks.pdf indexing from tasks.org")
    print("=" * 70)
    print()

    success = test_tasks_pdf_indexing()

    print()
    print("=" * 70)
    if success:
        print("✓ TEST PASSED: tasks.pdf is properly indexed")
    else:
        print("✗ TEST FAILED: tasks.pdf is not indexed or found")
    print("=" * 70)

    sys.exit(0 if success else 1)
