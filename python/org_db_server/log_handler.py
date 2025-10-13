"""Custom log handler to keep recent logs in memory."""
import logging
from collections import deque
from typing import List

class MemoryLogHandler(logging.Handler):
    """Log handler that keeps recent log records in memory."""

    def __init__(self, capacity: int = 100):
        """Initialize with a fixed capacity."""
        super().__init__()
        self.capacity = capacity
        self.records = deque(maxlen=capacity)

    def emit(self, record: logging.LogRecord):
        """Store the log record."""
        try:
            msg = self.format(record)
            self.records.append({
                "timestamp": record.asctime if hasattr(record, 'asctime') else "",
                "level": record.levelname,
                "message": msg,
                "name": record.name
            })
        except Exception:
            self.handleError(record)

    def get_recent_logs(self, n: int = 10) -> List[dict]:
        """Get the last N log records."""
        return list(self.records)[-n:]

# Global instance
_memory_handler = None

def get_memory_handler() -> MemoryLogHandler:
    """Get or create the global memory log handler."""
    global _memory_handler
    if _memory_handler is None:
        _memory_handler = MemoryLogHandler(capacity=100)
        formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
        _memory_handler.setFormatter(formatter)

        # Add to root logger
        root_logger = logging.getLogger()
        root_logger.addHandler(_memory_handler)

    return _memory_handler
