# ontrack-4k
# Copyright (C) 2023  Alexander Kraus <nr4@z10.info>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


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
