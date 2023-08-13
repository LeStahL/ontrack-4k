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

include("${CMAKE_CURRENT_LIST_DIR}/common.cmake")

find_or_download_if_not_present(CRINKLER Crinkler.exe "https://github.com/runestubbe/Crinkler/releases/download/v2.3/crinkler23.zip" crinkler23/Win64/)

set(CMAKE_LINKER ${CRINKLER})
set(CMAKE_ASM_NASM_LINK_FLAGS "${CMAKE_ASM_NASM_LINK_FLAGS} /progressgui /compmode:veryslow /HASHTRIES:2400 /ORDERTRIES:20000 /TINYIMPORT /UNSAFEIMPORT /UNALIGNCODE")
