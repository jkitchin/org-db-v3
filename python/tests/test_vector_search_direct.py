#!/usr/bin/env python3
"""Direct test of vector_top_k() to isolate the issue."""

import libsql
import numpy as np
import os
from pathlib import Path
from sentence_transformers import SentenceTransformer

# Get database path
cache_dir = os.path.expanduser("~/Dropbox/emacs/cache/org-db-v3")
db_path = os.path.join(cache_dir, "org-db-v3.db")

print(f"Testing database: {db_path}")
print(f"Database exists: {Path(db_path).exists()}")

# Connect to database
conn = libsql.connect(db_path)
cursor = conn.cursor()

# Check embeddings
cursor.execute("SELECT COUNT(*) FROM embeddings")
embedding_count = cursor.fetchone()[0]
print(f"\nTotal embeddings: {embedding_count}")

# Check vector index
cursor.execute("SELECT name, type FROM sqlite_master WHERE type='index' AND name LIKE '%vector%'")
indexes = cursor.fetchall()
print(f"\nVector indexes:")
for idx in indexes:
    print(f"  - {idx[0]} ({idx[1]})")

# Generate embedding for "mushroom"
print("\nGenerating embedding for 'mushroom'...")
model = SentenceTransformer('all-MiniLM-L6-v2')
query_embedding = model.encode("mushroom")
print(f"  Embedding shape: {query_embedding.shape}")
print(f"  Embedding dtype: {query_embedding.dtype}")

# Convert to bytes
query_bytes = query_embedding.astype(np.float32).tobytes()
print(f"  Query bytes length: {len(query_bytes)}")

# Test 1: Simple vector_top_k call
print("\n=== Test 1: Simple vector_top_k call ===")
try:
    cursor.execute("""
        SELECT vt.id, vt.distance
        FROM vector_top_k('idx_embeddings_vector', ?, ?) vt
        LIMIT 5
    """, [query_bytes, 5])
    rows = cursor.fetchall()
    print(f"✓ Success! Found {len(rows)} results")
    for row in rows:
        print(f"  ID: {row[0]}, Distance: {row[1]}")
except Exception as e:
    print(f"✗ Failed: {e}")

# Test 2: With JOIN to embeddings
print("\n=== Test 2: With JOIN to embeddings ===")
try:
    cursor.execute("""
        SELECT vt.id, e.embedding_model
        FROM vector_top_k('idx_embeddings_vector', ?, ?) vt
        JOIN embeddings e ON e.rowid = vt.id
        LIMIT 5
    """, [query_bytes, 5])
    rows = cursor.fetchall()
    print(f"✓ Success! Found {len(rows)} results")
    for row in rows:
        print(f"  ID: {row[0]}, Model: {row[1]}")
except Exception as e:
    print(f"✗ Failed: {e}")

# Test 3: Full query like in search.py
print("\n=== Test 3: Full query (like search.py) ===")
try:
    model_name = "all-MiniLM-L6-v2"
    num_candidates = 10

    cursor.execute("""
        SELECT
            vt.id as chunk_id,
            c.chunk_text,
            c.chunk_type,
            c.begin_line,
            c.end_line,
            f.filename,
            lf.file_path as linked_file_path,
            lf.file_type as linked_file_type,
            e.embedding_vector
        FROM vector_top_k('idx_embeddings_vector', ?, ?) vt
        JOIN embeddings e ON e.rowid = vt.id
        JOIN chunks c ON c.rowid = e.chunk_id
        JOIN files f ON c.filename_id = f.rowid
        LEFT JOIN linked_files lf ON c.linked_file_id = lf.rowid
        WHERE e.embedding_model = ?
    """, [query_bytes, num_candidates, model_name])
    rows = cursor.fetchall()
    print(f"✓ Success! Found {len(rows)} results")
    for i, row in enumerate(rows[:3], 1):
        print(f"\n  Result {i}:")
        print(f"    Chunk ID: {row[0]}")
        print(f"    Text: {row[1][:100]}...")
        print(f"    Filename: {row[5]}")
except Exception as e:
    print(f"✗ Failed: {e}")
    import traceback
    traceback.print_exc()

conn.close()
print("\nDone!")
