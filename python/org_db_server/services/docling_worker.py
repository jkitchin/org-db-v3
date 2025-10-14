#!/usr/bin/env python3
"""Isolated worker process for Docling conversions.

This runs in a separate process to prevent Docling crashes from taking down
the main server. If Docling crashes, only this worker dies and can be restarted.
"""
import os
import sys
import json
import hashlib
from pathlib import Path

# Disable tokenizer parallelism to avoid fork warnings
os.environ['TOKENIZERS_PARALLELISM'] = 'false'

from docling.document_converter import DocumentConverter


def calculate_md5(file_path: str) -> str:
    """Calculate MD5 hash of a file."""
    hash_md5 = hashlib.md5()
    with open(file_path, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash_md5.update(chunk)
    return hash_md5.hexdigest()


def convert_file(file_path: str, max_file_size: int = 52428800) -> dict:
    """Convert a file to markdown using Docling.

    Returns JSON result on stdout.
    """
    try:
        path = Path(file_path)

        # Check file exists
        if not path.exists():
            return {
                "status": "error",
                "error": "File not found",
                "md5": None,
                "file_size": 0
            }

        # Get file size
        file_size = path.stat().st_size

        if file_size > max_file_size:
            return {
                "status": "skipped",
                "error": f"File too large: {file_size / 1024 / 1024:.1f}MB",
                "md5": None,
                "file_size": file_size
            }

        # Calculate MD5
        try:
            md5 = calculate_md5(file_path)
        except Exception as e:
            return {
                "status": "error",
                "error": f"Error calculating MD5: {str(e)}",
                "md5": None,
                "file_size": file_size
            }

        # Convert document
        try:
            converter = DocumentConverter()
            result = converter.convert(file_path)
            markdown_text = result.document.export_to_markdown()

            # Cleanup
            del result
            del converter
            import gc
            gc.collect()

            return {
                "status": "success",
                "markdown": markdown_text,
                "md5": md5,
                "file_size": file_size
            }

        except Exception as e:
            return {
                "status": "error",
                "error": str(e),
                "md5": md5,
                "file_size": file_size
            }

    except Exception as e:
        return {
            "status": "error",
            "error": f"Unexpected error: {str(e)}",
            "md5": None,
            "file_size": 0
        }


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(json.dumps({"status": "error", "error": "No file path provided"}))
        sys.exit(1)

    file_path = sys.argv[1]
    max_size = int(sys.argv[2]) if len(sys.argv) > 2 else 52428800

    result = convert_file(file_path, max_size)
    print(json.dumps(result))
    sys.exit(0 if result["status"] == "success" else 1)
