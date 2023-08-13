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

include("${CMAKE_CURRENT_LIST_DIR}/../external/cmake-find-or-download/find-or-download.cmake")

find_or_download_if_not_present(GLSLANG_VALIDATOR glslangValidator.exe "https://github.com/KhronosGroup/glslang/releases/download/master-tot/glslang-master-windows-x64-Release.zip" bin/)
find_or_download_if_not_present(SHADER_MINIFIER shader_minifier.exe "https://github.com/laurentlb/Shader_Minifier/releases/download/1.3.6/shader_minifier.exe" ./)
find_or_download_if_not_present(GO go.exe "https://go.dev/dl/go1.19.windows-amd64.zip" go1.19.windows-amd64.zip/go/bin/)
find_or_download_if_not_present(FFMPEG ffmpeg.exe "https://www.gyan.dev/ffmpeg/builds/ffmpeg-release-essentials.zip" ffmpeg-5.0-essentials_build/bin/)
find_or_download_if_not_present(NASM nasm.exe "https://www.nasm.us/pub/nasm/releasebuilds/2.15.05/win64/nasm-2.15.05-win64.zip" nasm-2.15.05/)
find_or_download_if_not_present(GLEXT glext.h "https://www.khronos.org/registry/OpenGL/api/GL/glext.h" ./)
find_or_download_if_not_present(KHRPLATFORM khrplatform.h "https://www.khronos.org/registry/EGL/api/KHR/khrplatform.h" ./)
file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/downloads/KHR")
file(COPY ${KHRPLATFORM} DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/downloads/KHR/")
file(COPY ${GLEXT} DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/downloads/")
include_directories("${CMAKE_CURRENT_BINARY_DIR}/downloads")

set(CMAKE_SIZEOF_VOID_P 4)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/external/cmake-modules/")
find_package(WindowsSDK REQUIRED COMPONENTS tools)
get_windowssdk_library_dirs(${WINDOWSSDK_LATEST_DIR} WINDOWSSDK_LIBRARY_DIRS)
link_directories(${WINDOWSSDK_LIBRARY_DIRS})

set(CMAKE_C_COMPILER clang)
set(CMAKE_ASM_NASM_COMPILER ${NASM})
set(CMAKE_ASM_NASM_FLAGS "-fwin32 -Ox")

# Linker
set(CMAKE_ASM_NASM_LINK_LIBRARY_SUFFIX ".lib")
set(CMAKE_ASM_NASM_LINK_LIBRARY_FLAG "")
set(CMAKE_ASM_NASM_LIBRARY_PATH_FLAG "/libpath:")
set(CMAKE_ASM_NASM_LINK_FLAGS /subsystem:windows)
set(CMAKE_ASM_NASM_LINK_EXECUTABLE "<CMAKE_LINKER> <CMAKE_ASM_NASM_LINK_FLAGS> <LINK_FLAGS> <OBJECTS> /out:<TARGET> <LINK_LIBRARIES>")
