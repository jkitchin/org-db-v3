# Supported Document Formats

Comprehensive list of file formats that can be indexed when linked from org files.

> **Note**: This project originally used Docling for document conversion, but it was **removed due to frequent segmentation faults** causing server crashes. We now use lightweight specialized libraries (pymupdf4llm, python-docx, python-pptx) with markitdown as a fallback. This approach is more stable and faster for most document types.

## Document Formats

| Format | Extensions | Notes |
|--------|------------|-------|
| **PDF** | `.pdf` | Full support including OCR for scanned PDFs |
| **Microsoft Word** | `.docx`, `.doc` | Text, tables, images |
| **Microsoft Excel** | `.xlsx`, `.xls` | Spreadsheet data converted to tables |
| **Microsoft PowerPoint** | `.pptx`, `.ppt` | Slides converted to sections |

## Web & Markup Formats

| Format | Extensions | Notes |
|--------|------------|-------|
| **HTML** | `.html`, `.htm`, `.xhtml` | Web pages with structure preserved |
| **Markdown** | `.md`, `.markdown` | Already markdown, minimal processing |
| **AsciiDoc** | `.asciidoc`, `.adoc` | Technical documentation format |

## Data Formats

| Format | Extensions | Notes |
|--------|------------|-------|
| **CSV** | `.csv` | Comma-separated values converted to tables |

## Image Formats (with OCR)

| Format | Extensions | Notes |
|--------|------------|-------|
| **PNG** | `.png` | Lossless image format |
| **JPEG** | `.jpg`, `.jpeg` | Compressed image format |
| **TIFF** | `.tiff`, `.tif` | High-quality scanned documents |
| **BMP** | `.bmp` | Windows bitmap |
| **WebP** | `.webp` | Modern web image format |

**OCR Capabilities**:
- Extracts text from images
- Handles scanned documents
- Multi-language support
- Table detection in images

## Audio Formats (ASR)

| Format | Extensions | Status | Notes |
|--------|------------|--------|-------|
| **WAV** | `.wav` | ⚠️ SKIPPED | Causes multiprocessing leaks |
| **MP3** | `.mp3` | ⚠️ SKIPPED | Causes multiprocessing leaks |
| **M4A** | `.m4a` | ⚠️ SKIPPED | Causes multiprocessing leaks |
| **OGG** | `.ogg` | ⚠️ SKIPPED | Causes multiprocessing leaks |

**ASR Status**:
- ⚠️ **Disabled by default** due to markitdown multiprocessing resource leaks
- Markitdown's speech recognition creates semaphore leaks that accumulate during directory indexing
- **Workaround**: Process audio files separately or enable with `DocumentConverter(skip_audio=False)`
- To re-enable: Modify `document_converter.py` and set `skip_audio=False`

## Specialized Formats

| Format | Extensions | Notes |
|--------|------------|-------|
| **WebVTT** | `.vtt` | Video subtitle format |
| **USPTO XML** | `.xml` | Patent documents |
| **JATS XML** | `.xml` | Journal Article Tag Suite (academic papers) |
| **Docling JSON** | `.json` | Docling's internal format |

## Format Categories by Use Case

### 📚 Academic/Research
- PDF (papers, theses)
- DOCX (manuscripts)
- JATS XML (journal articles)
- Images (scanned papers with OCR)

### 💼 Business/Office
- PDF (reports, presentations)
- DOCX (documents)
- XLSX (data, reports)
- PPTX (presentations)
- CSV (data exports)

### 🌐 Web Content
- HTML (web pages, documentation)
- Markdown (documentation, notes)
- AsciiDoc (technical docs)

### 🖼️ Visual/Media
- Images (scanned documents, screenshots)
- Audio (meeting recordings, lectures)
- VTT (video transcripts)

## Performance Considerations

### Fast (< 1 second)
- Markdown (`.md`)
- HTML (`.html`)
- CSV (`.csv`)
- Small PDFs (< 10 pages)

### Medium (1-5 seconds)
- DOCX
- XLSX
- PPTX
- Medium PDFs (10-50 pages)

### Slow (5-30 seconds)
- Large PDFs (50+ pages)
- Images with OCR
- Scanned PDFs

### Very Slow (> 30 seconds)
- Audio files with ASR
- Large image collections
- Complex spreadsheets

## File Size Limits

| Format | Recommended Max | Hard Limit |
|--------|----------------|------------|
| PDF | 20MB | 50MB |
| DOCX | 10MB | 50MB |
| XLSX | 5MB | 50MB |
| Images | 5MB each | 10MB |
| Audio | 50MB | 100MB |

**Default**: `org-db-v3-max-linked-file-size = 50MB`

## Configuration Examples

### Index Everything (except audio)
```elisp
(setq org-db-v3-index-linked-files t)
(setq org-db-v3-index-images t)
(setq org-db-v3-index-audio-files nil)  ; Too slow
```

### Office Documents Only
```elisp
(setq org-db-v3-linked-file-extensions
      '("pdf" "docx" "doc" "pptx" "ppt" "xlsx" "xls"))
```

### Academic Papers (PDF + Images with OCR)
```elisp
(setq org-db-v3-linked-file-extensions
      '("pdf" "png" "jpg" "jpeg" "tiff"))
(setq org-db-v3-index-images t)
```

### Web Documentation
```elisp
(setq org-db-v3-linked-file-extensions
      '("html" "htm" "md" "markdown" "asciidoc"))
```

## Common Org Link Examples

### Office Documents
```org
[[file:~/Documents/report.pdf][Q3 Report]]
[[file:../presentations/slides.pptx][Conference Talk]]
[[file:./data/analysis.xlsx][Sales Data]]
```

### Images (with OCR)
```org
[[file:~/scans/receipt.jpg][Receipt 2024-01-15]]
[[file:./screenshots/bug.png][Bug Screenshot]]
```

### Web Content
```org
[[file:~/docs/api.html][API Documentation]]
[[file:./notes.md][Meeting Notes]]
```

### Audio (if enabled)
```org
[[file:~/recordings/meeting-2024-01-15.mp3][Team Meeting]]
[[file:./lecture-01.wav][Lecture Recording]]
```

## Excluded Formats

Formats **NOT** supported by docling:

| Format | Reason |
|--------|--------|
| `.rtf` | Rich Text Format - not supported |
| `.odt`, `.ods`, `.odp` | LibreOffice formats - not supported |
| `.pages`, `.numbers`, `.key` | Apple iWork - not supported |
| `.epub` | eBooks - not supported (may add in future) |
| `.djvu` | Scanned documents - not supported |
| Video files (`.mp4`, `.avi`, etc.) | Only VTT subtitles supported |

## Implementation Priority

### Phase 1 (MVP)
- ✅ PDF
- ✅ DOCX
- ✅ XLSX
- ✅ PPTX

### Phase 2
- ✅ HTML
- ✅ Markdown
- ✅ Images (PNG, JPEG, TIFF)

### Phase 3
- 🔄 CSV
- 🔄 WebP, BMP
- 🔄 AsciiDoc

### Phase 4 (Advanced)
- ⏳ Audio (ASR)
- ⏳ WebVTT
- ⏳ Specialized XML

## Testing Matrix

| Format | Test File | Expected Chunks | Notes |
|--------|-----------|-----------------|-------|
| PDF | `sample.pdf` (5 pages) | ~20 | Standard document |
| DOCX | `report.docx` (10 pages) | ~30 | With tables |
| XLSX | `data.xlsx` (3 sheets) | ~15 | Multiple sheets |
| PPTX | `slides.pptx` (20 slides) | ~40 | Slide deck |
| HTML | `webpage.html` | ~10 | Web page |
| PNG | `scanned.png` | ~5 | OCR test |
| JPEG | `photo-with-text.jpg` | ~3 | OCR test |
| MD | `notes.md` | ~5 | Already markdown |

## Conversion Libraries

### Current Implementation (Stable)
- **pymupdf4llm**: PDF conversion (fast, reliable)
- **python-docx**: DOCX conversion
- **python-pptx**: PPTX conversion
- **markitdown**: Fallback for HTML, Excel, images, etc.

### Previous Implementation (Removed)
- ~~**Docling**~~ - Removed due to frequent segmentation faults
  - Issue: Caused server crashes during directory indexing
  - Impact: Unreliable for production use
  - Decision: Replaced with specialized libraries (October 2025)

## References

- [MarkItDown GitHub](https://github.com/microsoft/markitdown)
- [PyMuPDF4LLM Documentation](https://pymupdf.readthedocs.io/)
- [python-docx Documentation](https://python-docx.readthedocs.io/)
- [python-pptx Documentation](https://python-pptx.readthedocs.io/)
