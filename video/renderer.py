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

from PyQt6.QtOpenGLWidgets import QOpenGLWidget
from typing import Any
from OpenGL.GL import *
from PIL import Image
from os.path import join
from sys import exit

class Renderer(QOpenGLWidget):
    UNIFORM_LOCATION_IFRAME = 0
    UNIFORM_LOCATION_ISAMPLE = 1
    UNIFORM_LOCATION_IPASS = 2
    UNIFORM_LOCATION_IRESOLUTION = 3
    UNIFORM_LOCATION_ICHANNEL0 = 4

    SampleRate = 44100

    def __init__(self,
        shaderSource: str,
        width: int,
        height: int,
        rate: int,
        duration: int,
        output: str,
        parent: Any,
    ) -> None:
        super().__init__()

        self.shaderSource = shaderSource
        self._width = width
        self._height = height
        self.rate = rate
        self.duration = duration
        self._parent = parent
        self.output = output

        self.frame = 0
        self.textures = []
        self.framebuffers = []

        self.previewWidth = self.width()
        self.previewHeight = self.height()

    def initializeGL(self) -> None:
        self.shader = glCreateShader(GL_FRAGMENT_SHADER)
        glShaderSource(self.shader, self.shaderSource)
        glCompileShader(self.shader)
        self.program = glCreateProgram()
        glAttachShader(self.program, self.shader)
        glLinkProgram(self.program)

        glUseProgram(self.program)

        self.texture = glGenTextures(1)
        self.framebuffer = glGenFramebuffers(1)

    def paintGL(self) -> None:
        if self.frame == 0:
            glActiveTexture(GL_TEXTURE0)
            glBindTexture(GL_TEXTURE_2D, self.texture)

            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
            glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F, self._width, self._height, 0, GL_RGBA, GL_FLOAT, None)

            glBindFramebuffer(GL_FRAMEBUFFER, self.framebuffer)
            glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, self.texture, 0)
            
            glUniform1i(Renderer.UNIFORM_LOCATION_ICHANNEL0, 0)
       
        framebufferBinding = glGetIntegerv(GL_FRAMEBUFFER_BINDING)
        
        glUniform1i(Renderer.UNIFORM_LOCATION_IFRAME, self.frame)
        glUniform1i(Renderer.UNIFORM_LOCATION_ISAMPLE, int(self.frame * Renderer.SampleRate / self.rate))
        
        glBindFramebuffer(GL_FRAMEBUFFER, self.framebuffer)
        glUniform1i(Renderer.UNIFORM_LOCATION_IPASS, 0)
        glUniform2i(Renderer.UNIFORM_LOCATION_IRESOLUTION, self._width, self._height)
        glViewport(0, 0, self._width, self._height)
        glRecti(-1, -1, 1, 1)

        pixels = glGetTexImage(GL_TEXTURE_2D, 0, GL_RGB, GL_UNSIGNED_BYTE)
        image = Image.frombytes("RGB", (self._width, self._height), pixels)
        image = image.transpose( Image.FLIP_TOP_BOTTOM)
        image.save(join(self.output, "image.{}.png".format(self.frame)))

        glBindFramebuffer(GL_FRAMEBUFFER, framebufferBinding)
        glUniform1i(Renderer.UNIFORM_LOCATION_IPASS, 1)
        glUniform2i(Renderer.UNIFORM_LOCATION_IRESOLUTION, self.previewWidth, self.previewHeight)
        glViewport(0, 0, self.previewWidth, self.previewHeight)
        glRecti(-1, -1, 1, 1)

        self.frame += 1

        print(float(self.frame / self.rate), "/", float(self.duration))

        if float(self.frame / self.rate) < float(self.duration):
            self.update()
        else:
            exit(0)
            
    def resizeGL(self, w: int, h: int) -> None:
        self.previewWidth = w
        self.previewHeight = h
