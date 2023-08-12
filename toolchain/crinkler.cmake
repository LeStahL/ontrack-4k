include("${CMAKE_CURRENT_LIST_DIR}/common.cmake")

find_or_download_if_not_present(CRINKLER Crinkler.exe "https://github.com/runestubbe/Crinkler/releases/download/v2.3/crinkler23.zip" crinkler23/Win64/)

set(CMAKE_LINKER ${CRINKLER})
