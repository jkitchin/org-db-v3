"""FastAPI server for org-db v3."""
import os
import signal
import logging

# Disable tokenizer parallelism before any imports that use transformers
# This prevents warnings when forking subprocesses
os.environ['TOKENIZERS_PARALLELISM'] = 'false'

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import HTMLResponse
from pathlib import Path

from org_db_server.api import indexing, search, stats, agenda, linked_files
from org_db_server.log_handler import get_memory_handler

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Initialize memory log handler
memory_handler = get_memory_handler()
logger.info("org-db v3 server starting...")

app = FastAPI(title="org-db Server", version="0.1.0")

# Allow Emacs to connect from localhost
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Include routers
app.include_router(indexing.router)
app.include_router(search.router)
app.include_router(stats.router)
app.include_router(agenda.router)
app.include_router(linked_files.router)

@app.get("/health")
async def health_check():
    """Health check endpoint."""
    return {"status": "ok", "version": "0.1.0"}

@app.post("/api/shutdown")
async def shutdown():
    """Shutdown the server gracefully."""
    # Send SIGTERM to the parent process group
    os.kill(os.getpid(), signal.SIGTERM)
    return {"status": "shutting down"}

@app.get("/api/logs")
async def get_logs(lines: int = 10):
    """Get the last N lines of the server log."""
    logs = memory_handler.get_recent_logs(lines)
    return {"logs": logs, "count": len(logs)}

@app.get("/", response_class=HTMLResponse)
async def root():
    """Root endpoint - serve homepage."""
    # Load HTML template
    template_path = Path(__file__).parent / "templates" / "homepage.html"
    if template_path.exists():
        with open(template_path, "r") as f:
            return f.read()
    else:
        return {"message": "org-db v3 server running"}
