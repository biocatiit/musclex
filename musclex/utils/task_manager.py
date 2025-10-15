"""
Task manager for organizing parallel processing results.
Handles task submission, completion tracking, and result organization.
"""

import threading
import time
from dataclasses import dataclass, field
from typing import Optional, Dict, Any, List
from enum import Enum


class TaskStatus(Enum):
    """Status of a processing task"""
    PENDING = "pending"
    PROCESSING = "processing"
    COMPLETED = "completed"
    FAILED = "failed"


@dataclass
class ProcessingTask:
    """Container for tracking individual processing tasks"""
    filename: str
    job_index: int  # Original order in batch
    future: Optional[Any] = None
    status: TaskStatus = TaskStatus.PENDING
    result: Optional[Dict] = None
    error: Optional[str] = None
    submit_time: float = 0.0
    complete_time: float = 0.0
    
    @property
    def processing_time(self) -> float:
        """Calculate processing duration"""
        if self.complete_time > 0:
            return self.complete_time - self.submit_time
        return 0.0


class ProcessingTaskManager:
    """
    Manages task submission, completion tracking, and result organization.
    Thread-safe for use with concurrent futures callbacks.
    """
    
    def __init__(self):
        self.tasks: Dict[Any, ProcessingTask] = {}  # {future: ProcessingTask}
        self.completed_tasks: List[ProcessingTask] = []  # Completed tasks (ordered)
        self.pending_count = 0
        self.completed_count = 0
        self.failed_count = 0
        self.lock = threading.Lock()
    
    def submit_task(self, filename: str, job_index: int, future) -> ProcessingTask:
        """
        Register a new task submission.
        
        Args:
            filename: Name of the image file
            job_index: Position in original submission order
            future: concurrent.futures.Future object
        
        Returns:
            ProcessingTask object for tracking
        """
        task = ProcessingTask(
            filename=filename,
            job_index=job_index,
            future=future,
            status=TaskStatus.PROCESSING,
            submit_time=time.time()
        )
        
        with self.lock:
            self.tasks[future] = task
            self.pending_count += 1
        
        return task
    
    def complete_task(self, future, result: Dict, error: Optional[str] = None) -> Optional[ProcessingTask]:
        """
        Mark task as completed and organize result.
        
        Args:
            future: The completed future
            result: Result dictionary from worker
            error: Error message if task failed
        
        Returns:
            The completed ProcessingTask, or None if not found
        """
        with self.lock:
            if future not in self.tasks:
                return None
            
            task = self.tasks[future]
            task.complete_time = time.time()
            task.result = result
            task.error = error
            task.status = TaskStatus.COMPLETED if not error else TaskStatus.FAILED
            
            # Move to completed list and maintain order
            self.completed_tasks.append(task)
            self.completed_tasks.sort(key=lambda t: t.job_index)
            
            self.pending_count -= 1
            if error:
                self.failed_count += 1
            else:
                self.completed_count += 1
            
            # Remove from active tasks
            del self.tasks[future]
            
            return task
    
    def get_completed_in_order(self) -> List[ProcessingTask]:
        """
        Get all completed tasks in original submission order.
        
        Returns:
            List of ProcessingTask objects sorted by job_index
        """
        with self.lock:
            return self.completed_tasks.copy()
    
    def get_statistics(self) -> Dict[str, Any]:
        """
        Get current processing statistics.
        
        Returns:
            Dictionary with counts and timing information
        """
        with self.lock:
            total = len(self.completed_tasks) + self.pending_count
            avg_time = 0.0
            if self.completed_tasks:
                avg_time = sum(t.processing_time for t in self.completed_tasks) / len(self.completed_tasks)
            
            return {
                'total': total,
                'completed': self.completed_count,
                'failed': self.failed_count,
                'pending': self.pending_count,
                'avg_time': avg_time
            }
    
    def get_running_count(self) -> int:
        """Get the number of running tasks"""
        with self.lock:
            return sum(1 for task in self.tasks.values()
                       if task.future and task.future.running())
    
    def clear(self):
        """Clear all task tracking (for new batch)"""
        with self.lock:
            self.tasks.clear()
            self.completed_tasks.clear()
            self.pending_count = 0
            self.completed_count = 0
            self.failed_count = 0

