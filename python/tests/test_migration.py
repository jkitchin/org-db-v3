#!/usr/bin/env python3
"""Test script to verify database migration."""
import sys
from pathlib import Path
import sqlite3

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent))

from org_db_server.services.database import Database


def test_new_database():
    """Test migration with a brand new database."""
    print("=" * 60)
    print("Test 1: New Database")
    print("=" * 60)

    db_path = Path("/tmp/test_new_db.db")
    if db_path.exists():
        db_path.unlink()

    print(f"Creating new database at {db_path}")
    db = Database(db_path)

    # Check linked_files table exists
    cursor = db.conn.cursor()
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='linked_files'")
    result = cursor.fetchone()

    if result:
        print("✅ linked_files table created")
    else:
        print("❌ linked_files table NOT created")
        return False

    # Check chunks table has linked_file_id column
    cursor.execute("PRAGMA table_info(chunks)")
    columns = {row[1] for row in cursor.fetchall()}

    if 'linked_file_id' in columns:
        print("✅ chunks.linked_file_id column exists")
    else:
        print("❌ chunks.linked_file_id column NOT found")
        return False

    # Check schema_version table
    cursor.execute("SELECT version FROM schema_version ORDER BY version DESC LIMIT 1")
    result = cursor.fetchone()
    if result:
        print(f"✅ Schema version: {result[0]}")
    else:
        print("❌ Schema version not found")
        return False

    db.close()
    print()
    return True


def test_existing_database_migration():
    """Test migration with an existing database (simulated)."""
    print("=" * 60)
    print("Test 2: Existing Database Migration")
    print("=" * 60)

    db_path = Path("/tmp/test_existing_db.db")
    if db_path.exists():
        db_path.unlink()

    print(f"Creating old-style database at {db_path}")

    # Create an old-style database without linked_file_id
    conn = sqlite3.connect(str(db_path))
    conn.execute("PRAGMA foreign_keys = ON")

    # Create minimal old schema (without linked_files)
    conn.executescript("""
        CREATE TABLE files (
            rowid INTEGER PRIMARY KEY,
            filename TEXT UNIQUE NOT NULL,
            md5 TEXT NOT NULL,
            last_updated TEXT NOT NULL,
            file_size INTEGER,
            indexed_at TEXT
        );

        CREATE TABLE chunks (
            rowid INTEGER PRIMARY KEY,
            filename_id INTEGER NOT NULL,
            headline_id INTEGER,
            chunk_text TEXT NOT NULL,
            chunk_type TEXT,
            begin_line INTEGER NOT NULL,
            end_line INTEGER NOT NULL,
            char_offset INTEGER,
            FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE
        );

        -- Note: no linked_file_id column
    """)
    conn.commit()
    conn.close()

    print("Old database created (without linked_file_id)")

    # Check that linked_file_id doesn't exist
    conn = sqlite3.connect(str(db_path))
    cursor = conn.cursor()
    cursor.execute("PRAGMA table_info(chunks)")
    columns = {row[1] for row in cursor.fetchall()}
    conn.close()

    if 'linked_file_id' in columns:
        print("❌ linked_file_id should NOT exist in old database")
        return False
    else:
        print("✅ Confirmed: linked_file_id does not exist in old database")

    # Now open with Database class (should run migration)
    print("\nOpening database with migration system...")
    db = Database(db_path)

    # Check migration ran
    cursor = db.conn.cursor()

    # Check linked_files table exists
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='linked_files'")
    result = cursor.fetchone()
    if result:
        print("✅ linked_files table created by migration")
    else:
        print("❌ linked_files table NOT created")
        return False

    # Check chunks table now has linked_file_id column
    cursor.execute("PRAGMA table_info(chunks)")
    columns = {row[1] for row in cursor.fetchall()}

    if 'linked_file_id' in columns:
        print("✅ chunks.linked_file_id column added by migration")
    else:
        print("❌ chunks.linked_file_id column NOT added")
        return False

    # Check schema version
    cursor.execute("SELECT version FROM schema_version ORDER BY version DESC LIMIT 1")
    result = cursor.fetchone()
    if result and result[0] >= 1:
        print(f"✅ Schema version updated to: {result[0]}")
    else:
        print("❌ Schema version not updated correctly")
        return False

    db.close()
    print()
    return True


def test_foreign_key_constraint():
    """Test that foreign key constraints work correctly."""
    print("=" * 60)
    print("Test 3: Foreign Key Constraints")
    print("=" * 60)

    db_path = Path("/tmp/test_fk_db.db")
    if db_path.exists():
        db_path.unlink()

    db = Database(db_path)
    cursor = db.conn.cursor()

    # Create a test file
    cursor.execute("""
        INSERT INTO files (filename, md5, last_updated, file_size, indexed_at)
        VALUES ('test.org', 'abc123', '2024-01-01', 1000, '2024-01-01')
    """)
    file_id = cursor.lastrowid

    # Create a linked file
    cursor.execute("""
        INSERT INTO linked_files (org_file_id, org_link_line, file_path, file_type, file_size, md5, conversion_status)
        VALUES (?, 10, '/path/to/doc.pdf', 'pdf', 50000, 'def456', 'success')
    """, (file_id,))
    linked_file_id = cursor.lastrowid

    # Create a chunk referencing the linked file
    cursor.execute("""
        INSERT INTO chunks (filename_id, chunk_text, chunk_type, begin_line, end_line, char_offset, linked_file_id)
        VALUES (?, 'Test chunk from PDF', 'linked_file', 10, 10, 0, ?)
    """, (file_id, linked_file_id))

    db.conn.commit()

    print(f"✅ Created test data: file_id={file_id}, linked_file_id={linked_file_id}")

    # Verify the data
    cursor.execute("SELECT COUNT(*) FROM chunks WHERE linked_file_id = ?", (linked_file_id,))
    count = cursor.fetchone()[0]
    print(f"✅ Chunks with linked_file_id={linked_file_id}: {count}")

    # Test CASCADE delete
    cursor.execute("DELETE FROM linked_files WHERE rowid = ?", (linked_file_id,))
    db.conn.commit()

    cursor.execute("SELECT COUNT(*) FROM chunks WHERE linked_file_id = ?", (linked_file_id,))
    count = cursor.fetchone()[0]
    if count == 0:
        print(f"✅ Cascade delete worked: chunks deleted when linked_file deleted")
    else:
        print(f"❌ Cascade delete failed: {count} chunks still reference deleted linked_file")
        return False

    db.close()
    print()
    return True


if __name__ == "__main__":
    print("\n" + "=" * 60)
    print("Database Migration Tests")
    print("=" * 60)
    print()

    try:
        test1_passed = test_new_database()
        test2_passed = test_existing_database_migration()
        test3_passed = test_foreign_key_constraint()

        print("=" * 60)
        print("Test Results")
        print("=" * 60)
        print(f"Test 1 (New Database):      {'✅ PASS' if test1_passed else '❌ FAIL'}")
        print(f"Test 2 (Migration):         {'✅ PASS' if test2_passed else '❌ FAIL'}")
        print(f"Test 3 (Foreign Keys):      {'✅ PASS' if test3_passed else '❌ FAIL'}")
        print("=" * 60)

        all_passed = test1_passed and test2_passed and test3_passed
        if all_passed:
            print("✅ All tests passed!")
        else:
            print("❌ Some tests failed!")

        sys.exit(0 if all_passed else 1)

    except Exception as e:
        print(f"❌ Test error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
