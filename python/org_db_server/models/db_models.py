"""Database schema definitions."""

# SQL schema for org-db v3
SCHEMA = """
-- Core files table
CREATE TABLE IF NOT EXISTS files (
    rowid INTEGER PRIMARY KEY,
    filename TEXT UNIQUE NOT NULL,
    md5 TEXT NOT NULL,
    last_updated TEXT NOT NULL,
    file_size INTEGER,
    indexed_at TEXT
);

CREATE INDEX IF NOT EXISTS idx_files_filename ON files(filename);
CREATE INDEX IF NOT EXISTS idx_files_md5 ON files(md5);

-- Headlines table
CREATE TABLE IF NOT EXISTS headlines (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    title TEXT NOT NULL,
    level INTEGER NOT NULL,
    todo_keyword TEXT,
    todo_type TEXT,
    archivedp INTEGER,
    commentedp INTEGER,
    begin INTEGER NOT NULL,
    end INTEGER,
    tags TEXT,
    priority TEXT,
    scheduled TEXT,
    deadline TEXT,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_headlines_filename ON headlines(filename_id);
CREATE INDEX IF NOT EXISTS idx_headlines_todo ON headlines(todo_keyword);
CREATE INDEX IF NOT EXISTS idx_headlines_tags ON headlines(tags);

-- Tags table
CREATE TABLE IF NOT EXISTS tags (
    rowid INTEGER PRIMARY KEY,
    tag TEXT UNIQUE NOT NULL
);

-- Headline tags junction table
CREATE TABLE IF NOT EXISTS headline_tags (
    rowid INTEGER PRIMARY KEY,
    headline_id INTEGER NOT NULL,
    tag_id INTEGER NOT NULL,
    FOREIGN KEY(headline_id) REFERENCES headlines(rowid) ON DELETE CASCADE,
    FOREIGN KEY(tag_id) REFERENCES tags(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_headline_tags_headline ON headline_tags(headline_id);
CREATE INDEX IF NOT EXISTS idx_headline_tags_tag ON headline_tags(tag_id);

-- Properties table
CREATE TABLE IF NOT EXISTS properties (
    rowid INTEGER PRIMARY KEY,
    property TEXT UNIQUE NOT NULL
);

-- Headline properties table
CREATE TABLE IF NOT EXISTS headline_properties (
    rowid INTEGER PRIMARY KEY,
    headline_id INTEGER NOT NULL,
    property_id INTEGER NOT NULL,
    value TEXT,
    FOREIGN KEY(headline_id) REFERENCES headlines(rowid) ON DELETE CASCADE,
    FOREIGN KEY(property_id) REFERENCES properties(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_headline_properties_headline ON headline_properties(headline_id);
CREATE INDEX IF NOT EXISTS idx_headline_properties_property ON headline_properties(property_id);

-- Keywords table
CREATE TABLE IF NOT EXISTS keywords (
    rowid INTEGER PRIMARY KEY,
    keyword TEXT UNIQUE NOT NULL
);

-- File keywords table
CREATE TABLE IF NOT EXISTS file_keywords (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    keyword_id INTEGER NOT NULL,
    value TEXT,
    begin INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE,
    FOREIGN KEY(keyword_id) REFERENCES keywords(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_file_keywords_filename ON file_keywords(filename_id);

-- Links table
CREATE TABLE IF NOT EXISTS links (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    type TEXT,
    path TEXT,
    raw_link TEXT,
    description TEXT,
    search_option TEXT,
    begin INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_links_filename ON links(filename_id);
CREATE INDEX IF NOT EXISTS idx_links_type ON links(type);

-- Hashtags table
CREATE TABLE IF NOT EXISTS hashtags (
    rowid INTEGER PRIMARY KEY,
    hashtag TEXT UNIQUE NOT NULL
);

-- File hashtags table
CREATE TABLE IF NOT EXISTS file_hashtags (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    hashtag_id INTEGER NOT NULL,
    begin INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE,
    FOREIGN KEY(hashtag_id) REFERENCES hashtags(rowid) ON DELETE CASCADE
);

-- @labels table
CREATE TABLE IF NOT EXISTS atlabels (
    rowid INTEGER PRIMARY KEY,
    atlabel TEXT UNIQUE NOT NULL
);

-- File @labels table
CREATE TABLE IF NOT EXISTS file_atlabels (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    atlabel_id INTEGER NOT NULL,
    begin INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE,
    FOREIGN KEY(atlabel_id) REFERENCES atlabels(rowid) ON DELETE CASCADE
);

-- Email addresses table
CREATE TABLE IF NOT EXISTS email_addresses (
    rowid INTEGER PRIMARY KEY,
    email_address TEXT UNIQUE NOT NULL
);

-- File email addresses table
CREATE TABLE IF NOT EXISTS file_email_addresses (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    email_address_id INTEGER NOT NULL,
    begin INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE,
    FOREIGN KEY(email_address_id) REFERENCES email_addresses(rowid) ON DELETE CASCADE
);

-- Src blocks table
CREATE TABLE IF NOT EXISTS src_blocks (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    language TEXT,
    contents TEXT,
    begin INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_src_blocks_language ON src_blocks(language);

-- Timestamps table
CREATE TABLE IF NOT EXISTS timestamps (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    ts TEXT,
    type TEXT,
    context TEXT,
    begin INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE
);

-- Linked files table (for indexing linked documents: PDFs, DOCX, PPTX, etc.)
CREATE TABLE IF NOT EXISTS linked_files (
    rowid INTEGER PRIMARY KEY,
    org_file_id INTEGER NOT NULL,
    org_link_line INTEGER NOT NULL,
    file_path TEXT NOT NULL,
    file_type TEXT NOT NULL,
    file_size INTEGER,
    md5 TEXT NOT NULL,
    last_converted TEXT,
    conversion_status TEXT NOT NULL DEFAULT 'pending',
    conversion_error TEXT,
    indexed_at TEXT,
    FOREIGN KEY(org_file_id) REFERENCES files(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_linked_files_org_file ON linked_files(org_file_id);
CREATE INDEX IF NOT EXISTS idx_linked_files_path ON linked_files(file_path);
CREATE INDEX IF NOT EXISTS idx_linked_files_md5 ON linked_files(md5);

-- Chunks table for semantic search
CREATE TABLE IF NOT EXISTS chunks (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    headline_id INTEGER,
    chunk_text TEXT NOT NULL,
    chunk_type TEXT,
    begin_line INTEGER NOT NULL,
    end_line INTEGER NOT NULL,
    char_offset INTEGER,
    linked_file_id INTEGER,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE,
    FOREIGN KEY(headline_id) REFERENCES headlines(rowid) ON DELETE CASCADE,
    FOREIGN KEY(linked_file_id) REFERENCES linked_files(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_chunks_filename ON chunks(filename_id);
CREATE INDEX IF NOT EXISTS idx_chunks_headline ON chunks(headline_id);
CREATE INDEX IF NOT EXISTS idx_chunks_linked_file ON chunks(linked_file_id);

-- Embeddings table
-- Note: F32_BLOB(384) is required for libsql vector search (all-MiniLM-L6-v2 model)
CREATE TABLE IF NOT EXISTS embeddings (
    rowid INTEGER PRIMARY KEY,
    chunk_id INTEGER NOT NULL,
    embedding_model TEXT NOT NULL,
    embedding_vector F32_BLOB(384) NOT NULL,
    embedding_dim INTEGER NOT NULL,
    created_at TEXT,
    FOREIGN KEY(chunk_id) REFERENCES chunks(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_embeddings_chunk ON embeddings(chunk_id);
CREATE INDEX IF NOT EXISTS idx_embeddings_model ON embeddings(embedding_model);

-- Vector index for fast semantic search using libsql_vector_idx
CREATE INDEX IF NOT EXISTS idx_embeddings_vector ON embeddings(libsql_vector_idx(embedding_vector));

-- Images table
CREATE TABLE IF NOT EXISTS images (
    rowid INTEGER PRIMARY KEY,
    filename_id INTEGER NOT NULL,
    image_path TEXT NOT NULL,
    image_type TEXT,
    width INTEGER,
    height INTEGER,
    file_size INTEGER,
    begin INTEGER,
    ocr_text TEXT,
    FOREIGN KEY(filename_id) REFERENCES files(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_images_path ON images(image_path);

-- Image embeddings table (CLIP)
-- Note: F32_BLOB(512) is required for libsql vector search (clip-ViT-B-32 model)
CREATE TABLE IF NOT EXISTS image_embeddings (
    rowid INTEGER PRIMARY KEY,
    image_id INTEGER NOT NULL,
    clip_model TEXT NOT NULL,
    embedding_vector F32_BLOB(512) NOT NULL,
    embedding_dim INTEGER NOT NULL,
    created_at TEXT,
    FOREIGN KEY(image_id) REFERENCES images(rowid) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_image_embeddings_image ON image_embeddings(image_id);
CREATE INDEX IF NOT EXISTS idx_image_embeddings_model ON image_embeddings(clip_model);

-- Vector index for fast image search using libsql_vector_idx
CREATE INDEX IF NOT EXISTS idx_image_embeddings_vector ON image_embeddings(libsql_vector_idx(embedding_vector));

-- Full-text search virtual table
CREATE VIRTUAL TABLE IF NOT EXISTS fts_content USING fts5(
    filename,
    title,
    content,
    tags
);
"""
