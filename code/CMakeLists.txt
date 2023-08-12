set(CMAKE_INCLUDE_CURRENT_DIR on)

# Build sointu-compile
add_custom_command(
    COMMAND ${GO} build -o "${CMAKE_CURRENT_BINARY_DIR}/sointu-compile.exe" cmd/sointu-compile/main.go
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/sointu-compile.exe"
    WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}/external/sointu"
    COMMENT "Compiling Sointu yaml to asm compiler..."
)

# sointu-compile the track
add_custom_command(
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/sointu-compile.exe" -arch 386 -o song.asm "${PROJECT_SOURCE_DIR}/music/song.yml"
    OUTPUT
        "${CMAKE_CURRENT_BINARY_DIR}/song.asm"
        "${CMAKE_CURRENT_BINARY_DIR}/song.h"
    DEPENDS
        "${PROJECT_SOURCE_DIR}/music/song.yml"
        "${CMAKE_CURRENT_BINARY_DIR}/sointu-compile.exe"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Compiling sointu track YAML file to assembly..."
)

# Validate and minify graphics.
add_custom_command(
    COMMAND ${GLSLANG_VALIDATOR} "${PROJECT_SOURCE_DIR}/graphics/gfx.frag"
    COMMAND ${SHADER_MINIFIER} "${PROJECT_SOURCE_DIR}/graphics/gfx.frag" -o gfx.inc --format nasm -v
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/gfx.inc"
    DEPENDS "${PROJECT_SOURCE_DIR}/graphics/gfx.frag"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Validating and minifying graphics..."
)

set(sources
    song.asm
    intro.asm
    gfx.inc
)

# Add a target for the demo
add_executable(${PROJECT_NAME}-1920x1080 ${sources})
target_compile_definitions(${PROJECT_NAME}-1920x1080 PRIVATE WIDTH=1920 HEIGHT=1080)
target_link_libraries(${PROJECT_NAME}-1920x1080 opengl32 kernel32 user32 gdi32 winmm)