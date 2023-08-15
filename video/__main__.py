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

from argparse import ArgumentParser
from sys import (
    argv,
    exit,
)
from PyQt6.QtWidgets import QApplication
from .preview import Preview

if __name__ == '__main__':
    parser = ArgumentParser('ontrack-4k video rendering tool (c) 2023 Alexander Kraus <nr4@z10.info>.')
    parser.add_argument('-o,--output', dest='output', required=True, help='Directory to render frames to.')
    parser.add_argument('-s,--shader', dest='shader', required=True, help='Shader source file to render to video.')
    parser.add_argument('-w,--width', type=int, dest='width', required=True, help='Width of video.')
    parser.add_argument('-j,--height', type=int, dest='height', required=True, help='Height of video.')
    parser.add_argument('-r,--rate', type=int, dest='rate', default=60, help='Frame rate for the video.')
    parser.add_argument('-d,--duration', type=int, dest='duration', required=True, help='Duration of the video rendering.')
    args = parser.parse_args()

    app = QApplication(argv)

    shaderSource = ""
    with open(args.shader, 'rt') as f:
        shaderSource = f.read()

    preview = Preview(shaderSource, args.width, args.height, args.rate, args.duration, args.output)
    preview.show()

    app.exec()

    exit(0)
