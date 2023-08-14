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

cmake_minimum_required(VERSION 3.25)
project(ontrack-4k ASM_NASM)
find_package(OpenGL REQUIRED)
add_subdirectory(code)

# Automatic video rendering; only enabled if python is present.
find_package(Python)
if(Python_FOUND)
    set(VIDEO_DURATION 85)

    make_directory("${PROJECT_BINARY_DIR}/frames")
    make_directory("${PROJECT_BINARY_DIR}/video")
    
    add_custom_target(frames
        COMMAND ${Python_EXECUTABLE} -m poetry run python -m video -o "${PROJECT_BINARY_DIR}/frames" -s "${PROJECT_SOURCE_DIR}/graphics/gfx.frag" -w 1920 -j 1080 -d ${VIDEO_DURATION} -r 60
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Rendering video frames..."
    )

    add_custom_target(music
        COMMAND wav
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Rendering music wave file..."
    )

    add_custom_target(video
        COMMAND ${FFMPEG} -framerate 60 -i frames/image.%d.png -f f32le -ac 2 -ar 44100 -i msx.raw "video/${PROJECT_NAME}-1920x1080.mp4" -y
    )
    add_dependencies(video frames music)
endif()
