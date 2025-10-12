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
        # Split by blank lines (paragraphs) while tracking original positions
        current_line = 1  # Line numbers start at 1
        current_pos = 0

        # Find all paragraph boundaries
        blank_line_pattern = re.compile(r'\n\s*\n')

        for match in blank_line_pattern.finditer(text):
            # Extract paragraph from current position to blank line
            para_text = text[current_pos:match.start()]

            if para_text.strip():
                # Count lines from start of text to start of this paragraph
                lines_before = text[:current_pos].count('\n')
                begin_line = lines_before + 1  # +1 because lines start at 1

                # Count lines in this paragraph
                line_count = para_text.count('\n') + 1
                end_line = begin_line + line_count - 1

                chunks.append({
                    "text": para_text.strip(),
                    "chunk_type": "paragraph",
                    "begin_line": begin_line,
                    "end_line": end_line
                })

            # Move past the blank line(s)
            current_pos = match.end()

        # Handle the last paragraph (after the last blank line)
        if current_pos < len(text):
            para_text = text[current_pos:]
            if para_text.strip():
                lines_before = text[:current_pos].count('\n')
                begin_line = lines_before + 1
                line_count = para_text.count('\n') + 1
                end_line = begin_line + line_count - 1

                chunks.append({
                    "text": para_text.strip(),
                    "chunk_type": "paragraph",
                    "begin_line": begin_line,
                    "end_line": end_line
                })

    elif method == "fixed":
        # Fixed-size chunks with overlap, tracking absolute line numbers
        pos = 0

        while pos < len(text):
            end_pos = min(pos + chunk_size, len(text))
            chunk_text = text[pos:end_pos]

            # Calculate absolute line numbers
            lines_before = text[:pos].count('\n')
            begin_line = lines_before + 1  # Lines start at 1

            # Count lines in this chunk
            line_count = chunk_text.count('\n')
            end_line = begin_line + line_count

            chunks.append({
                "text": chunk_text,
                "chunk_type": "fixed",
                "begin_line": begin_line,
                "end_line": end_line
            })

            pos = end_pos - chunk_overlap if end_pos < len(text) else end_pos

    return chunks
