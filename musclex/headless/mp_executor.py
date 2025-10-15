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
        args: tuple of (settings, paramInfo, dir_path, filename, spec)
            - settings: dict with processing settings
            - paramInfo: dict with parameter information
            - dir_path: str directory path
            - filename: str display name of the image
            - spec: tuple loader spec like ("tiff", path) or ("h5", path, frame_idx)
    
    Returns:
        dict: {
            'filename': str,
            'info': dict (EquatorImage.info with all processing results),
            'error': str or None
        }
    """
    filename = None  # Initialize for error handling
    try:
        settings, paramInfo, dir_path, filename, spec = args
        
        # Create a minimal parent object that only provides statusPrint
        # We don't use EquatorWindowh here because it requires complex initialization
        class MinimalParent:
            def statusPrint(self, text):
                if text and text.strip():
                    import os
                    pid = os.getpid()
                    print(f"[Worker {pid}] {text}")
        
        parent = MinimalParent()
        
        # Load image using the spec
        from musclex.utils.file_manager import load_image_via_spec
        img = load_image_via_spec(dir_path, filename, spec)
        
        # Create and process EquatorImage with minimal parent
        from musclex.modules.EquatorImage import EquatorImage
        bioImg = EquatorImage(img, dir_path, filename, parent)
        
        # Process the image
        bioImg.process(settings, paramInfo)
        
        # Return results including processed images for UI preview
        return {
            'filename': filename,
            'info': bioImg.info,  # All results stored here
            'image': bioImg.image,  # Processed image (with mask applied)
            'rotated_img': bioImg.getRotatedImage(),  # Rotated image for display
            'error': None
        }
    
    except Exception as e:
        # Capture full traceback for debugging
        error_msg = traceback.format_exc()
        fname_str = filename if filename else "unknown"
        print(f"[ERROR] Failed to process {fname_str}:\n{error_msg}")
        
        return {
            'filename': fname_str,
            'info': None,
            'error': error_msg
        }

