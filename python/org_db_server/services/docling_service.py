"""Service for converting linked files to markdown using docling."""
import logging
import hashlib
import os
from pathlib import Path
from typing import Optional, Dict, Any
from docling.document_converter import DocumentConverter

logger = logging.getLogger(__name__)

# Enable HuggingFace downloads for PDF processing models
# PDF processing requires downloading AI models from HuggingFace Hub
if 'HF_HUB_OFFLINE' not in os.environ:
    os.environ['HF_HUB_OFFLINE'] = '0'


class DoclingService:
    """Service for converting documents to markdown using docling."""

    # Supported file extensions (from docling documentation)
    SUPPORTED_EXTENSIONS = {
        # Office documents
        '.pdf', '.docx', '.doc', '.xlsx', '.xls', '.pptx', '.ppt',
        # Web/markup formats
        '.html', '.htm', '.xhtml', '.md', '.markdown', '.asciidoc', '.adoc',
        # Data formats
        '.csv',
        # Image formats (with OCR)
        '.png', '.jpg', '.jpeg', '.tiff', '.tif', '.bmp', '.webp',
        # Specialized formats
        '.vtt', '.xml', '.json',
    }

    def __init__(self):
        """Initialize the docling service."""
        self._converter: Optional[DocumentConverter] = None
        logger.info("DoclingService initialized")

    @property
    def converter(self) -> DocumentConverter:
        """Lazy-load the document converter on first use."""
        if self._converter is None:
            logger.info("Loading docling DocumentConverter...")
            self._converter = DocumentConverter()
            logger.info("DocumentConverter loaded successfully")
        return self._converter

    @staticmethod
    def calculate_md5(file_path: str) -> str:
        """Calculate MD5 hash of a file for change detection."""
        hash_md5 = hashlib.md5()
        with open(file_path, "rb") as f:
            for chunk in iter(lambda: f.read(4096), b""):
                hash_md5.update(chunk)
        return hash_md5.hexdigest()

    def is_supported(self, file_path: str) -> bool:
        """Check if a file extension is supported by docling."""
        ext = Path(file_path).suffix.lower()
        return ext in self.SUPPORTED_EXTENSIONS

    def convert_to_markdown(
        self,
        file_path: str,
        max_file_size: int = 52428800  # 50MB default
    ) -> Dict[str, Any]:
        """
        Convert a document to markdown using docling.

        Args:
            file_path: Path to the file to convert
            max_file_size: Maximum file size in bytes (default 50MB)

        Returns:
            Dictionary with:
                - status: 'success', 'error', 'skipped'
                - markdown: The converted markdown text (if successful)
                - md5: MD5 hash of the source file
                - error: Error message (if failed)
                - file_size: Size of the file in bytes
        """
        path = Path(file_path)

        # Check file exists
        if not path.exists():
            logger.warning(f"File not found: {file_path}")
            return {
                "status": "error",
                "error": "File not found",
                "md5": None,
                "file_size": 0
            }

        # Check file size
        file_size = path.stat().st_size
        if file_size > max_file_size:
            logger.warning(f"File too large: {file_path} ({file_size} bytes)")
            return {
                "status": "skipped",
                "error": f"File too large: {file_size / 1024 / 1024:.1f}MB",
                "md5": None,
                "file_size": file_size
            }

        # Check extension
        if not self.is_supported(file_path):
            ext = path.suffix
            logger.warning(f"Unsupported file extension: {ext}")
            return {
                "status": "error",
                "error": f"Unsupported file extension: {ext}",
                "md5": None,
                "file_size": file_size
            }

        # Calculate MD5
        try:
            md5 = self.calculate_md5(file_path)
        except Exception as e:
            logger.error(f"Error calculating MD5 for {file_path}: {e}")
            return {
                "status": "error",
                "error": f"Error calculating MD5: {str(e)}",
                "md5": None,
                "file_size": file_size
            }

        # Convert document
        try:
            logger.info(f"Converting {file_path} to markdown...")
            result = self.converter.convert(file_path)

            # Export to markdown
            markdown_text = result.document.export_to_markdown()

            logger.info(f"Successfully converted {file_path} ({len(markdown_text)} chars)")
            return {
                "status": "success",
                "markdown": markdown_text,
                "md5": md5,
                "file_size": file_size
            }

        except Exception as e:
            logger.error(f"Error converting {file_path}: {e}", exc_info=True)
            return {
                "status": "error",
                "error": str(e),
                "md5": md5,
                "file_size": file_size
            }


# Global instance
_docling_service: Optional[DoclingService] = None


def get_docling_service() -> DoclingService:
    """Get or create the global docling service instance."""
    global _docling_service
    if _docling_service is None:
        _docling_service = DoclingService()
    return _docling_service
