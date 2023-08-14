from PyQt6.uic import loadUi
from PyQt6.QtWidgets import (
    QMainWindow,
    QWidget,
)
from PyQt6.QtCore import (
    Qt,
)
from typing import (
    Optional,
)
from .renderer import Renderer
from os.path import (
    dirname,
    join,
)

class Preview(QMainWindow):
    def __init__(self,
        shaderSource: str,
        width: int,
        height: int,
        rate: int,
        duration: int,
        output: str,
        parent: Optional[QWidget] = None,
        flags: Qt.WindowType = Qt.WindowType.Window,
    ) -> None:
        super().__init__(parent, flags)

        loadUi(join(dirname(__file__), 'preview.ui'), self)

        self.renderer = Renderer(shaderSource, width, height, rate, duration, output, self)
        self.renderer.setMinimumSize(400, 320)
        self.centralWidget().layout().addWidget(self.renderer)

        self.rate = rate
        self.duration = duration
