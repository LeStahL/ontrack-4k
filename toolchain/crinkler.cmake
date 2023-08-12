include("${CMAKE_CURRENT_LIST_DIR}/common.cmake")

find_or_download_if_not_present(CRINKLER Crinkler.exe "https://github.com/runestubbe/Crinkler/releases/download/v2.3/crinkler23.zip" crinkler23/Win64/)

set(CMAKE_LINKER ${CRINKLER})
set(CMAKE_ASM_NASM_LINK_FLAGS "${CMAKE_ASM_NASM_LINK_FLAGS} /progressgui /compmode:veryslow /HASHTRIES:2400 /ORDERTRIES:20000 /TINYIMPORT /UNSAFEIMPORT /UNALIGNCODE")
