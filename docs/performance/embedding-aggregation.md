# Embedding Aggregation Strategies

## Goal: One Embedding Per File Instead of Per Chunk

**Problem:** 360,013 chunks = 360,013 embeddings (each 384 floats × 4 bytes = 1.5KB)
**Solution:** Aggregate to file-level embeddings = 1,301 embeddings (99.6% reduction!)

---

## Strategy 1: Mean Pooling (Most Common) ⭐

**How it works:**
- Generate embeddings for all chunks
- Average them element-wise
- Result: Single embedding representing entire file

**Pros:**
- Simple and fast
- Preserves overall semantic meaning
- Works well in practice
- Standard approach in NLP

**Cons:**
- Loses fine-grained location information
- Can't point to specific chunk/paragraph

**Implementation:**
```python
import numpy as np

def aggregate_embeddings_mean(chunk_embeddings: List[np.ndarray]) -> np.ndarray:
    """Average all chunk embeddings into one file embedding."""
    return np.mean(chunk_embeddings, axis=0)
```

**Search behavior:**
- Query matches overall document topic
- Good for: "Find documents about X"
- Bad for: "Find specific paragraph mentioning Y"

---

## Strategy 2: Weighted Mean Pooling

**How it works:**
- Weight chunks by importance (length, position, keywords)
- Take weighted average

**Pros:**
- Emphasizes important content
- Still maintains speed
- Better than simple mean

**Cons:**
- Requires heuristics for weighting
- More complex

**Implementation:**
```python
def aggregate_embeddings_weighted(
    chunk_embeddings: List[np.ndarray],
    chunk_texts: List[str]
) -> np.ndarray:
    """Weighted average based on chunk length."""
    weights = np.array([len(text) for text in chunk_texts])
    weights = weights / weights.sum()  # Normalize

    weighted_sum = np.zeros_like(chunk_embeddings[0])
    for emb, weight in zip(chunk_embeddings, weights):
        weighted_sum += emb * weight

    return weighted_sum
```

**Weighting strategies:**
- By length: Longer chunks get more weight
- By position: First/last chunks weighted higher
- By TF-IDF: Chunks with unique terms weighted higher
- By keywords: Chunks matching important terms

---

## Strategy 3: Max Pooling

**How it works:**
- Take maximum value for each dimension across all embeddings
- Captures "strongest" semantic signals

**Pros:**
- Preserves distinctive features
- Good for classification tasks

**Cons:**
- Can be noisy
- Less semantically coherent than mean
- Not commonly used for semantic search

**Implementation:**
```python
def aggregate_embeddings_max(chunk_embeddings: List[np.ndarray]) -> np.ndarray:
    """Take max value per dimension across chunks."""
    return np.max(chunk_embeddings, axis=0)
```

---

## Strategy 4: Hierarchical Embedding (Best Quality) 🏆

**How it works:**
- Concatenate all chunk texts
- Generate ONE embedding from the full document
- Let the model handle the aggregation

**Pros:**
- Highest quality representation
- Model understands full context
- No information loss from averaging

**Cons:**
- Model has max token limit (usually 256-512 tokens)
- Must truncate long documents
- Slower for very long texts

**Implementation:**
```python
def generate_file_embedding(
    full_text: str,
    embedding_service,
    max_length: int = 8000  # ~512 tokens
) -> np.ndarray:
    """Generate embedding from full document text."""
    # Truncate if too long
    if len(full_text) > max_length:
        # Take beginning and end for context
        half = max_length // 2
        text = full_text[:half] + " ... " + full_text[-half:]
    else:
        text = full_text

    return embedding_service.generate_embedding(text)
```

**For very long documents:**
```python
def generate_file_embedding_smart_truncate(
    full_text: str,
    embedding_service,
    max_length: int = 8000
) -> np.ndarray:
    """Smart truncation: first + last + important middle sections."""
    if len(full_text) <= max_length:
        return embedding_service.generate_embedding(full_text)

    # Take first 30%, last 20%, and extract 50% from middle
    first_portion = int(max_length * 0.3)
    last_portion = int(max_length * 0.2)
    middle_portion = max_length - first_portion - last_portion

    first = full_text[:first_portion]
    last = full_text[-last_portion:]

    # Extract important sentences from middle
    middle_text = full_text[first_portion:-last_portion]
    # Simple heuristic: sentences with more unique words
    middle = extract_important_text(middle_text, middle_portion)

    combined = first + "\n...\n" + middle + "\n...\n" + last
    return embedding_service.generate_embedding(combined)
```

---

## Strategy 5: Hybrid: File + Chunk Embeddings (Recommended) 🎯

**How it works:**
- Store ONE file-level embedding for broad search
- Store SPARSE chunk embeddings for precision
- Use file embedding first, then drill down to chunks

**Pros:**
- Best of both worlds
- Fast initial search (file-level)
- Precise results (chunk-level for top matches)
- Huge database savings

**Cons:**
- More complex implementation
- Two-stage search

**Implementation:**

### Schema changes:
```sql
-- Add file-level embedding
ALTER TABLE files ADD COLUMN file_embedding F32_BLOB(384);
CREATE INDEX idx_files_embedding ON files(libsql_vector_idx(file_embedding));

-- Keep chunks table but make embeddings optional
-- Or store only for top N files
```

### Indexing:
```python
def index_file_hybrid(file_content: str, file_id: int):
    # 1. Generate file-level embedding from full/summarized content
    file_embedding = generate_file_embedding(file_content, max_length=8000)
    db.store_file_embedding(file_id, file_embedding)

    # 2. For chunked search, only store chunks for:
    #    - Important files (flagged by user)
    #    - Recent files (last 90 days)
    #    - Small files (< 100KB)
    if should_store_chunks(file_id):
        chunks = chunk_text(file_content)
        chunk_embeddings = generate_embeddings(chunks)
        db.store_chunks(file_id, chunks, chunk_embeddings)
```

### Searching:
```python
def hybrid_search(query: str, limit: int = 10):
    query_embedding = generate_embedding(query)

    # Stage 1: Fast file-level search
    top_files = vector_search_files(query_embedding, limit=50)

    # Stage 2: Chunk-level search within top files
    results = []
    for file in top_files[:10]:  # Top 10 files
        if file.has_chunks:
            # Search chunks within this file
            chunks = vector_search_chunks(query_embedding, file_id=file.id, limit=5)
            results.extend(chunks)
        else:
            # Return file-level match
            results.append(file)

    return results[:limit]
```

---

## Strategy 6: Summary-Based Embedding

**How it works:**
- Generate a summary of the document (LLM or extractive)
- Embed the summary instead of full text
- Summary captures key concepts

**Pros:**
- High quality representation
- Naturally handles long documents
- Works well for academic papers, reports

**Cons:**
- Requires summarization step (slow/expensive)
- Quality depends on summarizer
- May miss details

**Implementation:**
```python
def generate_summary_embedding(full_text: str, embedding_service) -> np.ndarray:
    # Option 1: Extractive summary (fast)
    summary = extract_key_sentences(full_text, max_sentences=10)

    # Option 2: LLM summary (better quality, slower)
    # summary = llm.summarize(full_text, max_length=500)

    return embedding_service.generate_embedding(summary)

def extract_key_sentences(text: str, max_sentences: int = 10) -> str:
    """Simple extractive summarization."""
    sentences = text.split('. ')

    # Score by: length, position, keyword density
    scores = []
    for i, sent in enumerate(sentences):
        score = 0
        score += len(sent.split())  # Longer sentences
        score += 10 if i < 3 else 0  # First sentences
        score += 10 if i > len(sentences) - 3 else 0  # Last sentences
        # Add TF-IDF or keyword matching here
        scores.append((score, sent))

    # Take top N
    scores.sort(reverse=True)
    summary_sents = [sent for _, sent in scores[:max_sentences]]

    return '. '.join(summary_sents)
```

---

## Strategy 7: CLS Token (For Transformer Models)

**How it works:**
- Some models (BERT-based) have a special [CLS] token
- This token embedding represents the entire sequence
- Already built into the model

**Pros:**
- Model-native aggregation
- No manual pooling needed
- High quality

**Cons:**
- Only works with certain models
- Sentence-transformers usually already do this

**Implementation:**
```python
# Most sentence-transformer models already return pooled embedding
# But if you have access to raw transformer:

from transformers import AutoModel, AutoTokenizer

def get_cls_embedding(text: str, model, tokenizer) -> np.ndarray:
    """Get CLS token embedding (already aggregated)."""
    inputs = tokenizer(text, return_tensors="pt",
                      truncation=True, max_length=512)
    outputs = model(**inputs)

    # CLS token is first token
    cls_embedding = outputs.last_hidden_state[0, 0, :].detach().numpy()

    return cls_embedding
```

**Note:** sentence-transformers already handles this internally!

---

## Comparison Table

| Strategy | Quality | Speed | DB Size Reduction | Best For |
|----------|---------|-------|-------------------|----------|
| **Mean Pooling** | Good | Fast | 99.6% | General purpose ⭐ |
| **Weighted Mean** | Better | Fast | 99.6% | Variable chunk importance |
| **Max Pooling** | Fair | Fast | 99.6% | Classification |
| **Hierarchical** | Best | Slower | 99.6% | Full doc understanding 🏆 |
| **Hybrid** | Excellent | Medium | 90-95% | Best of both worlds 🎯 |
| **Summary-Based** | Good | Slow | 99.6% | Long documents |
| **CLS Token** | Good | Fast | 99.6% | Already built-in |

---

## Recommendations by Use Case

### **Use Case 1: "Find files about topic X"**
→ **Hierarchical Embedding** (Strategy 4)
- One embedding per file from full text
- Perfect for "which documents discuss machine learning?"

### **Use Case 2: "Find specific paragraph or quote"**
→ **Hybrid Approach** (Strategy 5)
- File-level for initial search
- Chunk-level for precision
- Best balance of speed and accuracy

### **Use Case 3: "Maximum database reduction, basic search"**
→ **Mean Pooling** (Strategy 1)
- Simplest to implement
- Good enough for most searches
- 99.6% size reduction

### **Use Case 4: "High quality, willing to wait"**
→ **Summary-Based** (Strategy 6)
- For academic papers, reports
- Generate smart summaries
- Best semantic representation

---

## Implementation Priority

### **Phase 1: Quick Win (This Week)**
Implement **Mean Pooling** for linked files only:

```python
# In indexing.py, replace per-chunk embeddings with file-level

def index_linked_file_with_aggregation(file_content: str, file_id: int):
    # Generate chunks for processing
    chunks = chunk_text(file_content, method="fixed", chunk_size=2048)

    # Generate embeddings for all chunks
    texts = [c['text'] for c in chunks]
    chunk_embeddings = embedding_service.generate_embeddings(texts)

    # Aggregate to single embedding
    file_embedding = np.mean(chunk_embeddings, axis=0)

    # Store ONLY the aggregated embedding, not individual chunks
    db.store_linked_file_embedding(
        linked_file_id=linked_file_id,
        embedding=file_embedding,
        model_name=embedding_service.model_name
    )
```

**Result:** 5,164 embeddings instead of 350,000 (98.5% reduction!)

### **Phase 2: Better Quality (Next Week)**
Implement **Hierarchical Embedding**:

```python
def index_linked_file_hierarchical(file_content: str, file_id: int):
    # Smart truncation to fit model
    truncated = smart_truncate(file_content, max_length=8000)

    # Single embedding from full context
    file_embedding = embedding_service.generate_embedding(truncated)

    db.store_linked_file_embedding(linked_file_id, file_embedding)
```

### **Phase 3: Hybrid System (Future)**
Implement **Hybrid Approach** for best results:
- File-level embeddings for all files
- Chunk-level embeddings for recent/important files only
- Two-stage search

---

## Database Schema Changes

### **Option A: File-level only (Maximum reduction)**

```sql
-- Add file_embedding column to linked_files table
ALTER TABLE linked_files ADD COLUMN file_embedding F32_BLOB(384);
ALTER TABLE linked_files ADD COLUMN embedding_model TEXT;
CREATE INDEX idx_linked_files_embedding
    ON linked_files(libsql_vector_idx(file_embedding));

-- Remove chunks for linked files (or don't create them)
-- Keep chunks only for org files
```

### **Option B: Hybrid (Recommended)**

```sql
-- Add file_embedding to files table for all files
ALTER TABLE files ADD COLUMN file_embedding F32_BLOB(384);
CREATE INDEX idx_files_embedding
    ON files(libsql_vector_idx(file_embedding));

-- Keep chunks table but add priority flag
ALTER TABLE chunks ADD COLUMN priority INTEGER DEFAULT 0;

-- Only store chunks for priority files
-- Everyone gets file-level embedding
-- Important files also get chunk-level
```

---

## Expected Impact

### **Current State:**
- 360,013 chunks × 1.5KB each = ~540 MB (raw embeddings)
- With indexes: ~1.5-2 GB
- Plus chunk text storage

### **With File-Level Aggregation:**
- 6,465 file embeddings (1,301 org + 5,164 linked)
- 6,465 × 1.5KB = ~9.7 MB (raw embeddings)
- With indexes: ~30-40 MB
- **98.5% reduction in embedding storage!**

### **With Hybrid (90% reduction):**
- 6,465 file embeddings: ~10 MB
- ~35,000 chunk embeddings (priority files only): ~53 MB
- Total: ~63 MB vs 540 MB (88% reduction)

---

## Migration Path

1. **Add file_embedding columns** to schema
2. **Reindex** with file-level aggregation
3. **Keep old chunks** temporarily for comparison
4. **Test search quality** vs chunked approach
5. **Drop chunk embeddings** for linked files if satisfied
6. **Optimize** database with VACUUM

---

## Code Example: Complete Implementation

See next document: `implement_file_level_embeddings.py`
