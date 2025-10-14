#!/usr/bin/env python3
"""Test script to verify document converter installation and conversion."""
import sys
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent))

from org_db_server.services.document_converter import get_document_converter


def test_markdown_conversion():
    """Test converting a simple markdown file."""
    # Create a test markdown file
    test_file = Path("/tmp/test_document_converter.md")
    test_content = """# Test Document

This is a test document for document conversion.

## Section 1

Some text with **bold** and *italic* formatting.

## Section 2

- Item 1
- Item 2
- Item 3

## Conclusion

This is the end of the test document.
"""
    test_file.write_text(test_content)
    print(f"Created test file: {test_file}")
    print(f"File size: {test_file.stat().st_size} bytes")
    print()

    # Test conversion
    service = get_document_converter()
    print("Testing document conversion...")
    print()

    result = service.convert_to_markdown(str(test_file))

    print(f"Status: {result['status']}")
    print(f"MD5: {result.get('md5', 'N/A')}")
    print(f"File size: {result.get('file_size', 0)} bytes")
    print()

    if result['status'] == 'success':
        markdown = result['markdown']
        print(f"✅ Conversion successful!")
        print(f"Output length: {len(markdown)} characters")
        print()
        print("First 500 characters of output:")
        print("-" * 60)
        print(markdown[:500])
        print("-" * 60)
        return True
    else:
        print(f"❌ Conversion failed!")
        print(f"Error: {result.get('error', 'Unknown error')}")
        return False


def test_supported_extensions():
    """Test checking supported extensions."""
    service = get_document_converter()

    print("\nSupported extensions:")
    print("-" * 60)
    extensions = sorted(service.SUPPORTED_EXTENSIONS)
    for i in range(0, len(extensions), 6):
        print("  " + "  ".join(f"{ext:8}" for ext in extensions[i:i+6]))
    print("-" * 60)
    print(f"Total: {len(extensions)} formats supported")
    print()

    # Test some extensions
    test_cases = [
        ("/path/to/file.pdf", True),
        ("/path/to/file.docx", True),
        ("/path/to/file.txt", False),
        ("/path/to/file.png", True),
        ("/path/to/file.mp4", False),
    ]

    print("Testing extension detection:")
    for path, expected in test_cases:
        result = service.is_supported(path)
        status = "✅" if result == expected else "❌"
        print(f"  {status} {path}: {result}")
    print()


if __name__ == "__main__":
    print("=" * 60)
    print("Document Converter Installation Test")
    print("=" * 60)
    print()

    try:
        test_supported_extensions()
        success = test_markdown_conversion()

        print()
        print("=" * 60)
        if success:
            print("✅ All tests passed! Document converter is working correctly.")
        else:
            print("❌ Tests failed. See errors above.")
        print("=" * 60)

        sys.exit(0 if success else 1)

    except Exception as e:
        print(f"❌ Test error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
