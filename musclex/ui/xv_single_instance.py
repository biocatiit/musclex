"""Cross-platform single-instance support for the X-Ray Viewer.

Qt maps ``QLocalServer`` to Unix-domain sockets on Unix and named pipes on
Windows.  Keeping the transport here avoids platform-specific process probing
and also gives callers a way to tell whether the existing viewer is responsive.
"""

from __future__ import annotations

import getpass
import hashlib
import json
import os
import struct
import sys
import time
from collections.abc import Callable
from pathlib import Path

from PySide6.QtCore import QLockFile, QStandardPaths, QTimer
from PySide6.QtNetwork import QLocalServer, QLocalSocket

PROTOCOL_VERSION = 1
MAX_MESSAGE_SIZE = 1024 * 1024
_HEADER = struct.Struct("!I")


class XVProtocolError(ValueError):
    """Raised when an XV IPC message is invalid."""


def encode_message(message: dict) -> bytes:
    """Encode one length-prefixed UTF-8 JSON message."""
    payload = json.dumps(message, ensure_ascii=False, separators=(",", ":")).encode(
        "utf-8"
    )
    if len(payload) > MAX_MESSAGE_SIZE:
        raise XVProtocolError("message is too large")
    return _HEADER.pack(len(payload)) + payload


def decode_message(packet: bytes) -> dict:
    """Decode exactly one length-prefixed message."""
    if len(packet) < _HEADER.size:
        raise XVProtocolError("incomplete message header")
    (length,) = _HEADER.unpack_from(packet)
    if length > MAX_MESSAGE_SIZE:
        raise XVProtocolError("message is too large")
    if len(packet) != _HEADER.size + length:
        raise XVProtocolError("incomplete or trailing message data")
    try:
        message = json.loads(packet[_HEADER.size :].decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError) as exc:
        raise XVProtocolError("message is not valid UTF-8 JSON") from exc
    if not isinstance(message, dict):
        raise XVProtocolError("message must be a JSON object")
    return message


def make_open_request(
    filepath: str | None = None,
    *,
    frame: int | None = None,
    dataset: str | None = None,
    raise_window: bool = True,
) -> dict:
    """Build a normalized request for the viewer."""
    request = {
        "version": PROTOCOL_VERSION,
        "action": "open",
        "raise": bool(raise_window),
    }
    if filepath:
        request["file"] = os.path.abspath(os.path.expanduser(filepath))
    if frame is not None:
        request["frame"] = int(frame)
    if dataset is not None:
        request["dataset"] = dataset
    return request


def validate_request(request: dict) -> None:
    """Validate fields that are independent of the viewer implementation."""
    if request.get("version") != PROTOCOL_VERSION:
        raise XVProtocolError("unsupported protocol version")
    if request.get("action") != "open":
        raise XVProtocolError("unsupported action")
    filepath = request.get("file")
    if filepath is not None and (not isinstance(filepath, str) or not filepath):
        raise XVProtocolError("file must be a non-empty string")
    frame = request.get("frame")
    if frame is not None and (isinstance(frame, bool) or not isinstance(frame, int)):
        raise XVProtocolError("frame must be an integer")
    if frame is not None and frame < 0:
        raise XVProtocolError("frame must be zero or greater")
    dataset = request.get("dataset")
    if dataset is not None and (not isinstance(dataset, str) or not dataset):
        raise XVProtocolError("dataset must be a non-empty string")


def _instance_scope() -> str:
    """Return a stable per-user, per-login-session instance scope."""
    user = str(getattr(os, "getuid", lambda: getpass.getuser())())
    session = next(
        (
            os.environ[name]
            for name in (
                "MUSCLEX_XV_SESSION",
                "XDG_SESSION_ID",
                "WAYLAND_DISPLAY",
                "DISPLAY",
                "SESSIONNAME",
            )
            if os.environ.get(name)
        ),
        "default",
    )
    return f"{user}:{session}"


def instance_name() -> str:
    """Return the local-server name, with an override for integration tests."""
    override = os.environ.get("MUSCLEX_XV_SERVER_NAME")
    if override:
        return override
    digest = hashlib.sha256(_instance_scope().encode("utf-8")).hexdigest()[:20]
    return f"musclex-xv-{digest}"


def _user_temp_dir() -> Path:
    temp_root = QStandardPaths.writableLocation(QStandardPaths.TempLocation)
    user_digest = hashlib.sha256(_instance_scope().encode("utf-8")).hexdigest()[:12]
    directory = Path(temp_root) / f"musclex-xv-{user_digest}"
    directory.mkdir(mode=0o700, parents=True, exist_ok=True)
    return directory


def _endpoint_name(name: str) -> str:
    """Use an explicit writable Unix socket path and a pipe name on Windows."""
    if os.name == "nt":
        return name
    digest = hashlib.sha256(name.encode("utf-8")).hexdigest()[:20]
    return str(_user_temp_dir() / f"{digest}.sock")


def lock_path(server_name: str | None = None) -> str:
    """Return a short, writable lock path for this endpoint."""
    name = server_name or instance_name()
    digest = hashlib.sha256(name.encode("utf-8")).hexdigest()[:20]
    return str(_user_temp_dir() / f"{digest}.lock")


class XVSingleInstance:
    """Own or contact the local XV endpoint.

    The lock is important on Windows, where multiple named-pipe servers can
    otherwise listen under the same name and receive connections unpredictably.
    """

    def __init__(self, name: str | None = None):
        self.name = _endpoint_name(name or instance_name())
        self._lock = QLockFile(lock_path(self.name))
        self._server: QLocalServer | None = None
        self._handler: Callable[[dict], None] | None = None
        self._validator: Callable[[dict], None] | None = None
        self._clients: dict[QLocalSocket, bytearray] = {}

    def try_forward(self, request: dict, timeout_ms: int = 750) -> tuple[bool, str]:
        """Send to a running viewer; return ``(connected, error_message)``."""
        validate_request(request)
        socket = QLocalSocket()
        socket.connectToServer(self.name)
        if not socket.waitForConnected(timeout_ms):
            return False, socket.errorString()

        packet = encode_message(request)
        if socket.write(packet) != len(packet) or not socket.waitForBytesWritten(
            timeout_ms
        ):
            socket.abort()
            return True, "failed to send request"

        response = bytearray()
        while len(response) < _HEADER.size:
            if not socket.waitForReadyRead(timeout_ms):
                socket.abort()
                return True, "viewer did not acknowledge request"
            response.extend(bytes(socket.readAll()))
        (length,) = _HEADER.unpack_from(response)
        if length > MAX_MESSAGE_SIZE:
            socket.abort()
            return True, "viewer returned an invalid acknowledgement"
        while len(response) < _HEADER.size + length:
            if not socket.waitForReadyRead(timeout_ms):
                socket.abort()
                return True, "viewer returned an incomplete acknowledgement"
            response.extend(bytes(socket.readAll()))
        try:
            acknowledgement = decode_message(bytes(response))
        except XVProtocolError as exc:
            return True, f"invalid acknowledgement: {exc}"
        if acknowledgement.get("ok") is True:
            return True, ""
        return True, str(acknowledgement.get("error", "viewer rejected request"))

    def try_lock(self, timeout_ms: int = 0) -> bool:
        """Try to become the sole server owner."""
        if self._lock.tryLock(timeout_ms):
            return True
        # QLockFile normally removes stale files itself, but an explicit retry
        # handles abrupt termination consistently across Qt/OS versions.
        if self._lock.removeStaleLockFile():
            return self._lock.tryLock(timeout_ms)
        return False

    def listen(
        self,
        handler: Callable[[dict], None],
        validator: Callable[[dict], None] | None = None,
    ) -> tuple[bool, str]:
        """Start accepting requests. The caller must already own the lock."""
        if not self._lock.isLocked():
            return False, "single-instance lock is not held"
        self._handler = handler
        self._validator = validator
        QLocalServer.removeServer(self.name)
        server = QLocalServer()
        try:
            server.setSocketOptions(QLocalServer.UserAccessOption)
        except AttributeError:
            pass
        server.newConnection.connect(self._accept_connections)
        if not server.listen(self.name):
            return False, server.errorString()
        self._server = server
        return True, ""

    def close(self) -> None:
        """Close the endpoint and release ownership."""
        if self._server is not None:
            self._server.close()
            QLocalServer.removeServer(self.name)
            self._server = None
        if self._lock.isLocked():
            self._lock.unlock()

    def _accept_connections(self) -> None:
        assert self._server is not None
        while self._server.hasPendingConnections():
            socket = self._server.nextPendingConnection()
            self._clients[socket] = bytearray()
            socket.readyRead.connect(lambda sock=socket: self._read_client(sock))
            socket.disconnected.connect(lambda sock=socket: self._drop_client(sock))

    def _drop_client(self, socket: QLocalSocket) -> None:
        self._clients.pop(socket, None)
        socket.deleteLater()

    def _read_client(self, socket: QLocalSocket) -> None:
        buffer = self._clients.get(socket)
        if buffer is None:
            return
        buffer.extend(bytes(socket.readAll()))
        if len(buffer) < _HEADER.size:
            return
        (length,) = _HEADER.unpack_from(buffer)
        if length > MAX_MESSAGE_SIZE:
            self._reply(socket, False, "message is too large")
            return
        packet_length = _HEADER.size + length
        if len(buffer) < packet_length:
            return
        if len(buffer) != packet_length:
            self._reply(socket, False, "only one request is allowed per connection")
            return
        try:
            request = decode_message(bytes(buffer))
            validate_request(request)
            if self._validator is not None:
                self._validator(request)
        except (XVProtocolError, ValueError) as exc:
            self._reply(socket, False, str(exc))
            return

        self._reply(socket, True)
        if self._handler is not None:
            # Acknowledge first, then let potentially slow image I/O run on the
            # GUI event loop without making the client appear unresponsive.
            QTimer.singleShot(0, lambda req=request: self._handler(req))

    def _reply(self, socket: QLocalSocket, ok: bool, error: str = "") -> None:
        response = {"version": PROTOCOL_VERSION, "ok": ok}
        if error:
            response["error"] = error
        socket.write(encode_message(response))
        socket.flush()
        socket.disconnectFromServer()


def parse_xv_arguments(arguments: list[str]) -> dict:
    """Parse arguments following the ``xv`` shortcut into an IPC request."""
    import argparse

    parser = argparse.ArgumentParser(
        prog=f"{arguments[0]} xv",
        description="Open an image in the running MuscleX X-Ray Viewer.",
    )
    parser.add_argument("image", nargs="?", help="image or HDF5 file to display")
    parser.add_argument(
        "-i", "--file", dest="file_option", help="image file to display"
    )
    parser.add_argument(
        "--reuse",
        action="store_true",
        help="reuse the running viewer (this is the default and is kept for compatibility)",
    )
    parser.add_argument(
        "--frame",
        type=int,
        help="zero-based frame number for an HDF5 image stack",
    )
    parser.add_argument(
        "--dataset",
        help="reserved NeXus/HDF5 dataset path (not yet supported by XV)",
    )
    options = parser.parse_args(arguments[2:])
    if options.image and options.file_option:
        parser.error("specify the image either positionally or with --file, not both")
    filepath = options.file_option or options.image
    if options.frame is not None and not filepath:
        parser.error("--frame requires an image file")
    if options.dataset is not None and not filepath:
        parser.error("--dataset requires an image file")
    request = make_open_request(
        filepath, frame=options.frame, dataset=options.dataset, raise_window=True
    )
    validate_request(request)
    return request


def _validate_viewer_request(request: dict) -> None:
    """Reject requests XV cannot represent before acknowledging them."""
    if request.get("dataset") is not None:
        raise XVProtocolError(
            "dataset-addressed NeXus images are not supported by XV yet; "
            "send the detector image/HDF5 file instead"
        )
    filepath = request.get("file")
    if filepath and not os.path.isfile(filepath):
        raise XVProtocolError(f"file does not exist: {filepath}")


def _open_in_viewer(window, request: dict) -> None:
    """Deliver a validated request to an ``XRayViewerGUI`` instance."""
    filepath = request.get("file")
    if filepath:
        window.navigator.load_from_file(filepath)
        frame = request.get("frame")
        if frame is not None:
            manager = window.file_manager
            if manager.current_file_type != "h5":
                raise XVProtocolError("--frame can only be used with an HDF5 file")
            frame_count = manager.current_h5_nframes or 0
            if frame >= frame_count:
                raise XVProtocolError(
                    f"frame {frame} is outside the file's 0-{frame_count - 1} range"
                )
            manager.current_frame_idx = frame
            manager._update_current_position()
            manager.load_current()
            window.navigator._load_current_image()

    if request.get("raise", True):
        if window.isMinimized():
            window.showNormal()
        window.show()
        window.raise_()
        window.activateWindow()


def run_xv(arguments: list[str], stylesheet_text: str) -> int:
    """Run XV as a single-instance application or forward to the owner."""
    from PySide6.QtWidgets import QApplication, QMessageBox

    request = parse_xv_arguments(arguments)
    try:
        _validate_viewer_request(request)
    except XVProtocolError as exc:
        print(f"musclex xv: {exc}", file=sys.stderr)
        return 1
    app = QApplication.instance() or QApplication([arguments[0]])
    app.setStyle("Fusion")
    app.setStyleSheet(stylesheet_text)

    instance = XVSingleInstance()
    connected, error = instance.try_forward(request)
    if connected:
        if error:
            print(f"musclex xv: {error}", file=sys.stderr)
            return 1
        return 0

    # QLockFile serializes startup. A process that loses the race waits for the
    # winner to finish constructing the GUI and begin listening.
    if not instance.try_lock():
        deadline = time.monotonic() + 15.0
        last_error = error
        while time.monotonic() < deadline:
            connected, last_error = instance.try_forward(request, timeout_ms=250)
            if connected:
                if last_error:
                    print(f"musclex xv: {last_error}", file=sys.stderr)
                    return 1
                return 0
            if instance.try_lock():
                break
        else:
            print(
                f"musclex xv: another viewer is starting but did not respond: {last_error}",
                file=sys.stderr,
            )
            return 1

    # Recheck after acquiring the lock in case an owner became available while
    # this process was waiting.
    connected, error = instance.try_forward(request, timeout_ms=100)
    if connected:
        instance.close()
        if error:
            print(f"musclex xv: {error}", file=sys.stderr)
            return 1
        return 0

    from musclex.ui.XRayViewerGUI import XRayViewerGUI

    window = XRayViewerGUI()

    def handle_request(incoming: dict) -> None:
        try:
            _open_in_viewer(window, incoming)
        except (
            Exception
        ) as exc:  # noqa: BLE001 - GUI boundary must report load failures
            QMessageBox.warning(window, "Unable to open image", str(exc))

    listening, error = instance.listen(handle_request, _validate_viewer_request)
    if not listening:
        instance.close()
        print(f"musclex xv: cannot start request listener: {error}", file=sys.stderr)
        return 1
    app.aboutToQuit.connect(instance.close)

    try:
        _open_in_viewer(window, request)
    except XVProtocolError as exc:
        instance.close()
        print(f"musclex xv: {exc}", file=sys.stderr)
        window.close()
        return 1
    return app.exec()
