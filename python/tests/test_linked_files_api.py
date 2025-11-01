#!/usr/bin/env python3
"""Test the linked files API endpoint."""
import sys
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent))

from org_db_server.services.database import Database
from org_db_server.services.document_converter import get_document_converter
from org_db_server.services.embeddings import get_embedding_service
from org_db_server.services.chunking import chunk_text


def test_linked_file_workflow():
    """Test the complete linked file indexing workflow."""
    print("=" * 60)
    print("Linked Files API Workflow Test")
    print("=" * 60)

    # Setup: Create test database and org file
    db_path = Path("/tmp/test_linked_files_api.db")
    semantic_path = Path("/tmp/test_linked_files_api_semantic.db")
    image_path = Path("/tmp/test_linked_files_api_image.db")
    if db_path.exists():
        db_path.unlink()
    if semantic_path.exists():
        semantic_path.unlink()
    if image_path.exists():
        image_path.unlink()

    print("\n1. Creating test database...")
    db = Database(db_path, semantic_path, image_path)

    # Create a test org file entry
    org_filename = "/tmp/test.org"
    org_file_id = db.get_or_create_file_id(org_filename, "abc123", 500)
    db.main_conn.commit()
    print(f"   ✅ Created org file entry (id={org_file_id})")

    # Create a test HTML file to link to
    test_html = Path("/tmp/test_linked.html")
    test_html.write_text("""
    <!DOCTYPE html>
    <html>
    <head><title>Test Document</title></head>
    <body>
        <h1>Test Linked Document</h1>
        <p>This is content from a linked file.</p>
        <h2>Section 1</h2>
        <p>Important information about project planning.</p>
        <h2>Section 2</h2>
        <p>More details about implementation.</p>
    </body>
    </html>
    """)
    print(f"   ✅ Created test HTML file: {test_html}")

    # Test document conversion
    print("\n2. Testing document conversion...")
    converter = get_document_converter()
    conversion = converter.convert_to_markdown(str(test_html))

    if conversion['status'] != 'success':
        print(f"   ❌ Conversion failed: {conversion.get('error')}")
        return False

    print(f"   ✅ Conversion successful")
    print(f"      Markdown length: {len(conversion['markdown'])} chars")
    print(f"      MD5: {conversion['md5']}")
    print(f"      File size: {conversion['file_size']} bytes")

    # Test chunking
    print("\n3. Testing chunking...")
    chunk_results = chunk_text(conversion['markdown'], method="paragraph")
    chunks = [c['text'] for c in chunk_results]
    print(f"   ✅ Created {len(chunks)} chunks")

    # Test embedding generation
    print("\n4. Testing embedding generation...")
    embedding_service = get_embedding_service()
    embeddings = embedding_service.generate_embeddings(chunks)
    print(f"   ✅ Generated {len(embeddings)} embeddings")
    print(f"      Embedding dimension: {len(embeddings[0])}")
    print(f"      Model: {embedding_service.model_name}")

    # Test database storage
    print("\n5. Testing database storage...")

    # Create linked file entry
    org_link_line = 10
    file_type = 'html'
    linked_file_id = db.get_or_create_linked_file(
        org_file_id=org_file_id,
        org_link_line=org_link_line,
        file_path=str(test_html),
        file_type=file_type,
        file_size=conversion['file_size'],
        md5=conversion['md5'],
        conversion_status='success'
    )
    db.main_conn.commit()
    print(f"   ✅ Created linked_file entry (id={linked_file_id})")

    # Store chunks
    chunk_dicts = chunk_results  # Use chunk_results directly which has the right format
    db.store_linked_file_chunks(
        org_filename=org_filename,
        org_link_line=org_link_line,
        linked_file_path=str(test_html),
        chunks=chunk_dicts,
        embeddings=embeddings,
        model_name="all-MiniLM-L6-v2"
    )
    print(f"   ✅ Stored {len(chunks)} chunks")

    # Verify storage
    print("\n6. Verifying storage...")

    # Chunks are stored in semantic_conn, not main_conn
    cursor = db.semantic_conn.cursor()

    # Check chunks exist (linked_file_path is used, not linked_file_id)
    cursor.execute("SELECT COUNT(*) FROM chunks WHERE linked_file_path = ?", (str(test_html),))
    chunk_count = cursor.fetchone()[0]
    print(f"   ✅ Chunks in database: {chunk_count}")

    # Check chunks point to org file (can't JOIN with files table - it's in a different DB)
    cursor.execute(
        """SELECT c.filename, c.begin_line
           FROM chunks c
           WHERE c.linked_file_path = ?
           LIMIT 1""",
        (str(test_html),)
    )
    row = cursor.fetchone()
    if row:
        print(f"   ✅ Chunks point to org file: {row[0]} (line {row[1]})")
    else:
        print("   ❌ No chunks found")
        return False

    # Check embeddings exist
    cursor.execute(
        """SELECT COUNT(*)
           FROM embeddings e
           JOIN chunks c ON e.chunk_id = c.rowid
           WHERE c.linked_file_path = ?""",
        (str(test_html),)
    )
    embedding_count = cursor.fetchone()[0]
    print(f"   ✅ Embeddings in database: {embedding_count}")

    # Test retrieval
    print("\n7. Testing retrieval...")
    linked_files = db.get_linked_files_for_org_file(org_file_id)
    print(f"   ✅ Found {len(linked_files)} linked files for org file")

    if linked_files:
        lf = linked_files[0]
        print(f"      - File: {lf['file_path']}")
        print(f"      - Type: {lf['file_type']}")
        print(f"      - Status: {lf['conversion_status']}")

    # Test get_linked_file_info
    info = db.get_linked_file_info(linked_file_id)
    if info:
        print(f"   ✅ Got linked file info")
        print(f"      - Org file: {info['org_filename']}")
        print(f"      - Line: {info['org_link_line']}")

    # Test semantic search would find these chunks
    print("\n8. Testing that chunks are searchable...")
    cursor.execute(
        """SELECT c.chunk_text
           FROM chunks c
           JOIN embeddings e ON c.rowid = e.chunk_id
           WHERE c.linked_file_path = ?
           LIMIT 1""",
        (str(test_html),)
    )
    row = cursor.fetchone()
    if row:
        print(f"   ✅ Chunk with embedding found:")
        print(f"      \"{row[0][:80]}...\"")
    else:
        print("   ❌ No searchable chunks found")
        return False

    db.close()

    print("\n" + "=" * 60)
    print("✅ All tests passed!")
    print("=" * 60)
    return True


if __name__ == "__main__":
    try:
        success = test_linked_file_workflow()
        sys.exit(0 if success else 1)
    except Exception as e:
        print(f"\n❌ Test error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
