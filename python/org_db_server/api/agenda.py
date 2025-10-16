"""Agenda API endpoints."""
from fastapi import APIRouter, HTTPException
from typing import List, Dict, Any
from datetime import datetime, timedelta
from pydantic import BaseModel, Field

from org_db_server.services.database import Database
from org_db_server.config import settings

router = APIRouter(prefix="/api", tags=["agenda"])

# Global database instance
db = Database(settings.db_path, settings.semantic_db_path, settings.image_db_path)


class AgendaRequest(BaseModel):
    """Request for agenda items."""
    before: str = Field(default="+2w", description="Show items before this date (e.g., +2w, +1m, 2025-12-31)")


class AgendaItem(BaseModel):
    """Single agenda item."""
    title: str
    filename: str
    begin: int
    level: int
    todo_keyword: str
    priority: str | None = None
    deadline: str | None = None
    scheduled: str | None = None
    tags: str | None = None


class AgendaResponse(BaseModel):
    """Response from agenda query."""
    results: List[AgendaItem]
    before: str


def parse_relative_date(date_str: str) -> str:
    """Parse relative date like '+2w' or '+1m' into ISO format."""
    date_str = date_str.strip()

    # If it's already a date, return it
    if date_str and date_str[0] not in ['+', '-']:
        return date_str

    # Parse relative date
    now = datetime.now()

    if date_str.startswith('+'):
        amount_str = date_str[1:-1]  # Remove + and unit
        unit = date_str[-1]

        try:
            amount = int(amount_str)
        except ValueError:
            return now.strftime("%Y-%m-%d")

        if unit == 'd':
            target = now + timedelta(days=amount)
        elif unit == 'w':
            target = now + timedelta(weeks=amount)
        elif unit == 'm':
            # Approximate month as 30 days
            target = now + timedelta(days=amount * 30)
        elif unit == 'y':
            # Approximate year as 365 days
            target = now + timedelta(days=amount * 365)
        else:
            target = now

        return target.strftime("%Y-%m-%d")

    return now.strftime("%Y-%m-%d")


@router.post("/agenda", response_model=AgendaResponse)
async def get_agenda(request: AgendaRequest):
    """Get agenda items (TODO tasks with deadlines or scheduled dates)."""
    try:
        cursor = db.main_conn.cursor()

        # Parse the before date
        before_date = parse_relative_date(request.before)

        # Query for deadline items
        cursor.execute("""
            SELECT
                h.title,
                f.filename,
                h.begin,
                h.level,
                h.todo_keyword,
                h.priority,
                h.deadline,
                NULL as scheduled,
                h.tags
            FROM headlines h
            JOIN files f ON h.filename_id = f.rowid
            WHERE h.todo_keyword = 'TODO'
            AND h.deadline IS NOT NULL
            AND date(h.deadline) <= date(?)
            ORDER BY h.deadline ASC
        """, (before_date,))

        deadline_items = cursor.fetchall()

        # Query for scheduled items
        cursor.execute("""
            SELECT
                h.title || ' (scheduled)' as title,
                f.filename,
                h.begin,
                h.level,
                h.todo_keyword,
                h.priority,
                NULL as deadline,
                h.scheduled,
                h.tags
            FROM headlines h
            JOIN files f ON h.filename_id = f.rowid
            WHERE h.todo_keyword = 'TODO'
            AND h.scheduled IS NOT NULL
            AND date(h.scheduled) <= date(?)
            ORDER BY h.scheduled ASC
        """, (before_date,))

        scheduled_items = cursor.fetchall()

        # Query for TODO items without deadline or scheduled
        cursor.execute("""
            SELECT
                h.title,
                f.filename,
                h.begin,
                h.level,
                h.todo_keyword,
                h.priority,
                NULL as deadline,
                NULL as scheduled,
                h.tags
            FROM headlines h
            JOIN files f ON h.filename_id = f.rowid
            WHERE h.todo_keyword = 'TODO'
            AND h.deadline IS NULL
            AND h.scheduled IS NULL
            ORDER BY h.priority DESC, f.filename, h.begin
        """)

        todo_items = cursor.fetchall()

        # Combine all items (deadline first, then scheduled, then other todos)
        all_items = deadline_items + scheduled_items + todo_items

        # Convert to AgendaItem objects
        results = [
            AgendaItem(
                title=row[0],
                filename=row[1],
                begin=row[2],
                level=row[3],
                todo_keyword=row[4],
                priority=row[5],
                deadline=row[6],
                scheduled=row[7],
                tags=row[8]
            )
            for row in all_items
        ]

        return AgendaResponse(
            results=results,
            before=request.before
        )

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))
