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
        
        # Create ImageData and EquatorImage with minimal parent
        from musclex.utils.image_data import ImageData
        from musclex.modules.EquatorImage import EquatorImage
        inpaint = settings.get('inpaint', False)
        from musclex.utils.settings_manager import SettingsManager
        settings_manager = SettingsManager(dir_path)
        image_data = ImageData(img=img, img_path=dir_path, img_name=filename, inpaint=inpaint,
                               settings_manager=settings_manager)
        bioImg = EquatorImage(image_data, parent)
        
        # Process the image
        bioImg.process(settings, paramInfo)
        
        # Return results including processed images for UI preview
        return {
            'filename': filename,
            'info': bioImg.info,
            'image': bioImg.image,
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


def process_one_qf_image(args):
    """
    Headless QuadrantFolder processing function (no Qt dependencies).

    Runs in a worker process. Loads the image, builds an ``ImageData``
    + ``QuadrantFolder`` from plain Python state, runs the full
    pipeline, and writes per-image artefacts to disk:

        - ``<output_dir>/qf_cache/<name>.info``           (via process())
        - ``<output_dir>/qf_results/<name>_folded[_compressed].tif``
        - ``<output_dir>/qf_results/bg/<name>.bg.tif``    (if bgsub != 'None')

    The caller (main process) is responsible for the things that can
    race or that need GUI state: ``qf_results/tasks_done.txt``, the
    aggregated ``background_sum.csv``, the QF summary CSV, the metrics
    CSV, and any progress / UI updates.

    Args:
        args: dict with the keys
            - dir_path (str): directory the image was loaded from
            - filename (str): display filename
            - spec: loader spec for ``load_image_via_spec``
            - flags (dict): processing flags (same shape as GUI getFlags())
            - bgsub (str): inside background subtraction method (string)
            - output_dir (str): where qf_cache / qf_results live
            - manual_center (tuple or None)
            - manual_rotation (float or None)
            - apply_blank (bool)
            - apply_mask (bool)
            - blank_weight (float)
            - inpaint (bool)
            - orientation_model (int)
            - detector (str or None)
            - compress_folded (bool)
            - job_index (int)

    Returns:
        dict with the keys
            - filename (str)
            - job_index (int)
            - info (dict): ``quadFold.info`` (small, picklable)
            - has_result (bool): whether ``resultImg`` was produced
            - center (tuple): reference center (used by CSV writer)
            - rotation (float): final rotation (used by CSV writer)
            - bg_sum (float or None): total bg intensity (for bg_sum CSV)
            - error (str or None): traceback if processing crashed
    """
    filename = None
    job_index = -1
    try:
        dir_path = args['dir_path']
        filename = args['filename']
        spec = args['spec']
        flags = args['flags']
        bgsub = args.get('bgsub', 'None')
        output_dir = args.get('output_dir') or dir_path
        manual_center = args.get('manual_center')
        manual_rotation = args.get('manual_rotation')
        apply_blank = bool(args.get('apply_blank', False))
        apply_mask = bool(args.get('apply_mask', False))
        blank_weight = float(args.get('blank_weight', 1.0))
        inpaint = bool(args.get('inpaint', False))
        orientation_model = int(args.get('orientation_model', 0) or 0)
        detector = args.get('detector')
        compress_folded = bool(args.get('compress_folded', True))
        job_index = int(args.get('job_index', -1))

        pid = os.getpid()

        # Minimal parent so QuadrantFolder.statusPrint / stop_process work.
        class MinimalParent:
            stop_process = False

            def statusPrint(self, text):
                if text and str(text).strip():
                    print(f"[QF worker {pid}] {filename}: {text}")

        parent = MinimalParent()

        from musclex.utils.file_manager import load_image_via_spec
        from musclex.utils.image_data import ImageData
        from musclex.modules.QuadrantFolder import QuadrantFolder
        from musclex.utils.settings_manager import SettingsManager

        img = load_image_via_spec(dir_path, filename, spec)
        if img is None:
            raise RuntimeError(f"load_image_via_spec returned None for {filename}")

        settings_manager = SettingsManager(dir_path)

        image_data = ImageData(
            img=img,
            img_path=dir_path,
            img_name=filename,
            center=tuple(manual_center) if manual_center is not None else None,
            rotation=manual_rotation,
            apply_blank=apply_blank,
            apply_mask=apply_mask,
            blank_weight=blank_weight,
            orientation_model=orientation_model,
            detector=detector,
            inpaint=inpaint,
            settings_manager=settings_manager,
        )

        quadFold = QuadrantFolder(image_data, parent=parent, output_dir=output_dir)
        quadFold.info['bgsub'] = bgsub
        quadFold.process(flags)

        has_result = 'resultImg' in quadFold.imgCache

        # ---- Write the folded result tif (per-image, no race) ----
        if has_result:
            try:
                _save_qf_result_image(quadFold, output_dir, compress_folded)
            except Exception as e:
                print(f"[QF worker {pid}] Failed to save folded image for {filename}: {e}")

        # ---- Write the background tif and compute bg_sum ----
        bg_sum = None
        try:
            bg_sum = _save_qf_background(quadFold, dir_path)
        except Exception as e:
            print(f"[QF worker {pid}] Failed to save background for {filename}: {e}")

        # ImageData.center triggers auto-calculation if needed; safe in worker.
        try:
            center = tuple(quadFold._image_data.center)
        except Exception:
            center = (0.0, 0.0)
        rotation = float(quadFold.rotation) if quadFold.rotation is not None else 0.0

        return {
            'filename': filename,
            'job_index': job_index,
            'info': dict(quadFold.info),
            'processing_flags': dict(quadFold.processing_flags) if isinstance(quadFold.processing_flags, dict) else {},
            'has_result': has_result,
            'center': center,
            'rotation': rotation,
            'bg_sum': bg_sum,
            'error': None,
        }

    except Exception:
        error_msg = traceback.format_exc()
        fname_str = filename if filename else "unknown"
        print(f"[QF worker ERROR] Failed to process {fname_str}:\n{error_msg}")
        return {
            'filename': fname_str,
            'job_index': job_index,
            'info': None,
            'processing_flags': {},
            'has_result': False,
            'center': None,
            'rotation': None,
            'bg_sum': None,
            'error': error_msg,
        }


def _save_qf_result_image(quadFold, output_dir, compress_folded):
    """Mirror QuadrantFoldingGUI.saveResults() so the worker can write the
    canonical _folded.tif itself (per-image filenames, no race)."""
    import fabio
    from PIL import Image
    from os.path import splitext, join
    from musclex.utils.file_manager import fullPath, createFolder

    result_path = fullPath(output_dir, 'qf_results')
    createFolder(result_path)

    base, _ = splitext(str(join(result_path, quadFold.img_name)))
    img = quadFold.imgCache['resultImg'].astype('float32')

    suffix = '_folded_compressed.tif' if compress_folded else '_folded.tif'
    out_file = base + suffix
    if compress_folded:
        Image.fromarray(img).save(out_file, compression='tiff_lzw')
    else:
        fabio.tifimage.tifimage(data=img).write(out_file)


def _save_qf_background(quadFold, dir_path):
    """Mirror FolderImageWorker._save_background() for the worker process.

    Returns ``np.sum(result_img)`` so the main process can append a single
    aggregated background_sum.csv after the batch finishes. Returns ``None``
    when the slow-path didn't run (fast-path) or when bgsub is disabled.
    """
    import numpy as np
    import fabio

    info = quadFold.info
    result = quadFold.imgCache.get('BgSubFold')
    avg_fold = quadFold.imgCache.get('avg_fold')

    if result is None or avg_fold is None:
        return None

    method = info.get('bgsub', 'None')
    if not method or method == 'None':
        return None

    from musclex.utils.background_search import makeFullImage
    background = avg_fold - result
    result_img = makeFullImage(background)

    if info.get('rotate'):
        result_img = np.rot90(result_img)

    bg_dir = os.path.join(dir_path, 'qf_results', 'bg')
    os.makedirs(bg_dir, exist_ok=True)

    result_path = os.path.join(bg_dir, f'{quadFold.img_name}.bg.tif')
    result_img = result_img.astype('float32')
    fabio.tifimage.tifimage(data=result_img).write(result_path)
    return float(np.sum(result_img))

