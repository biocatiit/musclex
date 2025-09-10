

class UIEventHandlerManager:
    def __init__(self, handers):
        self.handers = handers

    def add_hander(self, handler):
        self.handers.append(hander)

    def handle_mouse_press_event(self, mouse_event):
        for hanlder in self.handers:
            handler.handle_mouse_press_event(mouse_event)


