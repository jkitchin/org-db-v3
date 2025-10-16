# org-db v3 Documentation

Welcome to the org-db v3 documentation. This directory contains comprehensive documentation for the project.

## Documentation Structure

### Architecture
Core architectural decisions and system design:
- **[Multi-Database Architecture](architecture/multi-database.md)** - Separation of main, semantic, and image databases
- **[Linked Files](architecture/linked-files.md)** - Architecture and implementation of linked file indexing (PDF, DOCX, etc.)
- **[Supported Formats](architecture/supported-formats.md)** - Complete list of supported file formats for linked file indexing

### Features
Feature-specific documentation and guides:
- **[Property Search](features/property-search.md)** - Searching by org-mode properties
- **[Image Search](features/image-search.md)** - CLIP-based semantic image search
- **[Web Interface](features/web-interface.md)** - FastAPI web interface and homepage

### Performance
Performance optimization guides and benchmarks:
- **[ANN Search Optimization](performance/ann-search-optimization.md)** - Vector search optimization with libsql
- **[Database Bloat Prevention](performance/database-bloat-prevention.md)** - Strategies to prevent database size issues
- **[Embedding Aggregation](performance/embedding-aggregation.md)** - Different strategies for aggregating embeddings
- **[Chunking Comparison](performance/chunking-comparison.md)** - Analysis of different text chunking strategies
- **[Linked Files Optimization](performance/linked-files-optimization.md)** - Performance optimizations for linked file indexing

### Development
Development notes and implementation history:
- **[Design Notes](development/design-notes.org)** - Original design and planning notes
- **[2025-10-11 Implementation](development/2025-10-11-org-db-v3-implementation.md)** - Initial v3 implementation
- **[2025-10-12 Search Scope](development/2025-10-12-search-scope.md)** - Search scope feature implementation

## Quick Links

- **Main README**: [../README.org](../README.org)
- **Python Package README**: [../python/README.md](../python/README.md)
- **API Documentation**: Start the server and visit http://localhost:8765/docs

## Contributing

When adding new documentation:
1. Place it in the appropriate subdirectory (architecture/features/performance/development)
2. Update this README.md with a link and brief description
3. Use clear, descriptive filenames in kebab-case
4. Include code examples where appropriate
5. Add cross-references to related documentation
