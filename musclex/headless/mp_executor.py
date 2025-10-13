"""
Multiprocessing executor for parallel image processing.
Provides headless worker functions that process images without Qt dependencies.
"""

import os
import traceback


def init_worker():
    """
    Initialize worker process.
    Called once per child process at startup.
    Sets environment variables to prevent thread oversubscription.
    """
    # Limit threads per process to prevent N_processes × M_threads explosion
    os.environ["OMP_NUM_THREADS"] = "1"
    os.environ["MKL_NUM_THREADS"] = "1"
    os.environ["OPENBLAS_NUM_THREADS"] = "1"
    os.environ["NUMEXPR_NUM_THREADS"] = "1"
    
    # Print worker info
    pid = os.getpid()
    print(f"[Worker {pid}] Initialized with single-threaded BLAS/LAPACK")


def process_one_image(args):
    """
    Headless image processing function (no Qt dependencies).
    
    Args:
        args: tuple of (settings, paramInfo, file_manager, job_index)
            - settings: dict with processing settings
            - paramInfo: dict with parameter information
            - file_manager: FileManager instance for accessing image data
            - job_index: int index of the image to process
    
    Returns:
        dict: {
            'filename': str,
            'info': dict (EquatorImage.info with all processing results),
            'error': str or None
        }
    """
    try:
        settings, paramInfo, file_manager, job_index = args
        
        # Create a minimal parent object that only provides statusPrint
        # We don't use EquatorWindowh here because it requires complex initialization
        class MinimalParent:
            def statusPrint(self, text):
                if text and text.strip():
                    import os
                    pid = os.getpid()
                    print(f"[Worker {pid}] {text}")
        
        parent = MinimalParent()
        
        # Create and process EquatorImage with minimal parent
        from musclex.modules.EquatorImage import EquatorImage
        filename = file_manager.names[job_index]
        bioImg = EquatorImage(file_manager.get_image_by_index(job_index), file_manager.dir_path, filename, parent)
        
        # Process the image
        bioImg.process(settings, paramInfo)
        
        # Return results (no cache written by child)
        return {
            'filename': filename,
            'info': bioImg.info,  # All results stored here
            'error': None
        }
    
    except Exception as e:
        # Capture full traceback for debugging
        error_msg = traceback.format_exc()
        print(f"[ERROR] Failed to process {filename}:\n{error_msg}")
        
        return {
            'filename': filename,
            'info': None,
            'error': error_msg
        }

