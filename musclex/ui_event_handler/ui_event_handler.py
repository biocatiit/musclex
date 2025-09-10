from abc import ABC, abstractmethod
from musclex.ui_actions.ui_action_state import UIActionState


class UIAction(ABC):
    def __init__(self):
        self.state = UIActionState.INIT

    def handle_mouse_press_event(self, mouse_event):
        """
        mouse button is pressed.
        """
        pass

    def handle_mouse_relase_event(self, mouse_event):
        """
        mouse button is released.
        """
        pass

    def handle_mouse_move_event(self, mouse_event):
        """
        mouse moves.
        """
        pass

    def handle_mouse_scroll_event(self, mouse_event):
        """
        mouse scroll wheel is rolled.
        """
        pass

    def handle_figure_enter_event(self, location_event):
        """
        mouse enters a new figure.
        """
        pass

    def handle_figure_leave_event(self, location_event):
        """
        mouse leaves a figure.
        """
        pass

    def handle_axes_enter_event(self):
        """
        mouse enters a new axes.
        """
        pass

    def handle_axes_leave_event(self):
        """
        mouse leaves an axes.
        """
        pass
