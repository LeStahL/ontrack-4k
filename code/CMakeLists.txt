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

# Add a targets for the demo
# SD
add_executable(${PROJECT_NAME}-1280x720 ${sources})
target_compile_definitions(${PROJECT_NAME}-1280x720 PRIVATE WIDTH=1280 HEIGHT=720)
target_link_libraries(${PROJECT_NAME}-1280x720 opengl32 kernel32 user32 gdi32 winmm)
# HD
add_executable(${PROJECT_NAME}-1920x1080 ${sources})
target_compile_definitions(${PROJECT_NAME}-1920x1080 PRIVATE WIDTH=1920 HEIGHT=1080)
target_link_libraries(${PROJECT_NAME}-1920x1080 opengl32 kernel32 user32 gdi32 winmm)
# 4k
add_executable(${PROJECT_NAME}-3840x2160 ${sources})
target_compile_definitions(${PROJECT_NAME}-3840x2160 PRIVATE WIDTH=3840 HEIGHT=2160)
target_link_libraries(${PROJECT_NAME}-3840x2160 opengl32 kernel32 user32 gdi32 winmm)

# Pack release archive using the install mechanism
install(
    TARGETS
        ${PROJECT_NAME}-1280x720
        ${PROJECT_NAME}-1920x1080
        ${PROJECT_NAME}-3840x2160
    DESTINATION ${PROJECT_NAME}
)
install(
    FILES
        ${PROJECT_SOURCE_DIR}/NR4.nfo
    DESTINATION ${PROJECT_NAME}
)
install(CODE "
    execute_process(
        COMMAND ${CMAKE_COMMAND} -E tar \"cfv\" \"${CMAKE_INSTALL_PREFIX}/${PROJECT_NAME}.zip\" --format=zip \"${CMAKE_INSTALL_PREFIX}/${PROJECT_NAME}\"
        WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}
    )
")
