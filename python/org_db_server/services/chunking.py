"""Text chunking service."""
import re
from typing import List, Dict, Literal

def chunk_text(
    text: str,
    method: Literal["paragraph", "fixed"] = "paragraph",
    chunk_size: int = 512,
    chunk_overlap: int = 50
) -> List[Dict[str, any]]:
    """
    Chunk text into smaller pieces.

    Args:
        text: Text to chunk
        method: Chunking method ("paragraph" or "fixed")
        chunk_size: Maximum chunk size in characters (for fixed method)
        chunk_overlap: Overlap between chunks in characters

    Returns:
        List of chunk dictionaries with text, chunk_type, begin_line, end_line
    """
    chunks = []

    if method == "paragraph":
        # Split by blank lines (paragraphs)
        paragraphs = re.split(r'\n\s*\n', text)
        current_line = 0

        for para in paragraphs:
            if para.strip():
                line_count = para.count('\n') + 1
                chunks.append({
                    "text": para.strip(),
                    "chunk_type": "paragraph",
                    "begin_line": current_line,
                    "end_line": current_line + line_count
                })
                current_line += line_count + 1  # +1 for blank line

    elif method == "fixed":
        # Fixed-size chunks with overlap
        pos = 0
        line = 0

        while pos < len(text):
            end_pos = min(pos + chunk_size, len(text))
            chunk_text = text[pos:end_pos]

            # Count lines in chunk
            line_count = chunk_text.count('\n')

            chunks.append({
                "text": chunk_text,
                "chunk_type": "fixed",
                "begin_line": line,
                "end_line": line + line_count
            })

            line += line_count
            pos = end_pos - chunk_overlap if end_pos < len(text) else end_pos

    return chunks
