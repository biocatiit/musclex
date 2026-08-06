import multiprocessing
import os
import uuid

import pytest

pytest.importorskip("PySide6")

from musclex.ui.xv_single_instance import (
    PROTOCOL_VERSION,
    XVProtocolError,
    XVSingleInstance,
    _open_in_viewer,
    _validate_viewer_request,
    decode_message,
    encode_message,
    make_open_request,
    parse_xv_arguments,
    validate_request,
)


def _run_test_server(name, ready_queue, request_queue):
    from PySide6.QtCore import QCoreApplication, QTimer

    app = QCoreApplication([])
    instance = XVSingleInstance(name)
    if not instance.try_lock():
        ready_queue.put((False, "could not acquire the single-instance lock"))
        return

    def receive(request):
        request_queue.put(request)
        QTimer.singleShot(0, app.quit)

    listening, error = instance.listen(receive)
    ready_queue.put((listening, error))
    if listening:
        QTimer.singleShot(5000, app.quit)
        app.exec()
    instance.close()


def test_protocol_round_trip_handles_unicode_and_windows_paths():
    message = {
        "version": PROTOCOL_VERSION,
        "action": "open",
        "file": r"C:\data with spaces\α-image.tif",
        "raise": True,
    }

    assert decode_message(encode_message(message)) == message


@pytest.mark.parametrize(
    "message,error",
    [
        ({"version": 99, "action": "open"}, "version"),
        ({"version": 1, "action": "close"}, "action"),
        ({"version": 1, "action": "open", "frame": -1}, "frame"),
        ({"version": 1, "action": "open", "file": ""}, "file"),
    ],
)
def test_request_validation_rejects_invalid_messages(message, error):
    with pytest.raises(XVProtocolError, match=error):
        validate_request(message)


def test_decode_rejects_truncated_packet():
    packet = encode_message({"ok": True})

    with pytest.raises(XVProtocolError, match="incomplete"):
        decode_message(packet[:-1])


def test_parse_xv_arguments_supports_positional_file_and_frame(tmp_path):
    image = tmp_path / "image stack.h5"
    request = parse_xv_arguments(["musclex", "xv", str(image), "--frame", "17"])

    assert request == make_open_request(str(image), frame=17)
    assert request["file"] == os.path.abspath(image)


def test_parse_xv_arguments_supports_explicit_file_option(tmp_path):
    image = tmp_path / "image.tif"

    request = parse_xv_arguments(["musclex", "xv", "--file", str(image)])

    assert request["file"] == str(image)


def test_parse_xv_arguments_requires_file_for_frame():
    with pytest.raises(SystemExit):
        parse_xv_arguments(["musclex", "xv", "--frame", "2"])


def test_viewer_delivery_loads_requested_hdf5_frame(tmp_path):
    class Manager:
        current_file_type = "h5"
        current_h5_nframes = 20
        current_frame_idx = 0

        def _update_current_position(self):
            self.position_updated = True

        def load_current(self):
            self.current_loaded = True

    class Navigator:
        def __init__(self, manager):
            self.manager = manager

        def load_from_file(self, filepath):
            self.loaded_path = filepath

        def _load_current_image(self):
            self.image_loaded = True

    class Window:
        def __init__(self):
            self.file_manager = Manager()
            self.navigator = Navigator(self.file_manager)

        def isMinimized(self):
            return False

        def show(self):
            self.shown = True

        def raise_(self):
            self.raised = True

        def activateWindow(self):
            self.activated = True

    image = tmp_path / "stack.h5"
    image.touch()
    window = Window()

    _open_in_viewer(window, make_open_request(str(image), frame=17))

    assert window.navigator.loaded_path == str(image)
    assert window.file_manager.current_frame_idx == 17
    assert window.file_manager.position_updated
    assert window.file_manager.current_loaded
    assert window.navigator.image_loaded
    assert window.shown and window.raised and window.activated


def test_dataset_addressing_is_rejected_explicitly(tmp_path):
    image = tmp_path / "data.nxs"
    image.touch()

    with pytest.raises(XVProtocolError, match="dataset-addressed"):
        _validate_viewer_request(
            make_open_request(str(image), dataset="/entry/instrument/detector/data")
        )


def test_local_server_forwards_and_acknowledges_request():
    context = multiprocessing.get_context("spawn")
    ready_queue = context.Queue()
    request_queue = context.Queue()
    name = f"musclex-xv-test-{uuid.uuid4().hex}"
    process = context.Process(
        target=_run_test_server, args=(name, ready_queue, request_queue)
    )
    process.start()
    try:
        listening, error = ready_queue.get(timeout=10)
        assert listening, error

        request = make_open_request("unicode α image.tif")
        competing_instance = XVSingleInstance(name)
        assert competing_instance.try_lock() is False
        connected, error = competing_instance.try_forward(request, timeout_ms=2000)

        assert connected is True
        assert error == ""
        assert request_queue.get(timeout=10) == request
    finally:
        process.join(timeout=10)
        if process.is_alive():
            process.terminate()
            process.join(timeout=5)
    assert process.exitcode == 0
