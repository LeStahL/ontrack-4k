include("${CMAKE_CURRENT_LIST_DIR}/common.cmake")

find_program(LLD NAMES lld-link)

set(CMAKE_LINKER ${LLD})
