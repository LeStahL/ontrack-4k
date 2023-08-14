%define WAVE_MAPPER 0xFFFFFFFF
%define WHDR_PREPARED 0x2
%define NULL 0
%define FILE_ATTRIBUTE_NORMAL 0x00000080
%define CREATE_ALWAYS 2
%define GENERIC_WRITE 0x40000000
%define WAVE_FORMAT_IEEE_FLOAT 0x3

section music_file data
music_file:
    db "msx.raw", 0

section hfile bss
hfile:
    resd 1

section bytes_written bss
bytes_written:
    resd 1

; Music related data.
%define SAMPLE_SIZE 4
%define CHANNEL_COUNT 2
; This needs to be extracted manually from the header sointu generates.
%define SAMPLE_COUNT 3663360
%define SAMPLE_RATE 44100

section wavefmt data
wavefmt:
    dw WAVE_FORMAT_IEEE_FLOAT
    dw CHANNEL_COUNT
    dd SAMPLE_RATE 
    dd SAMPLE_SIZE * SAMPLE_RATE * CHANNEL_COUNT
    dw SAMPLE_SIZE * CHANNEL_COUNT
    dw SAMPLE_SIZE * 8
    dw 0

section wavehdr data
wavehdr:
    dd soundbuffer
    dd SAMPLE_COUNT * SAMPLE_SIZE * CHANNEL_COUNT
    times 2 dd 0
    dd WHDR_PREPARED
    times 4 dd 0
wavehdr_end:

section soundbuffer bss
soundbuffer:
    resd SAMPLE_COUNT * SAMPLE_SIZE * CHANNEL_COUNT

section hwaveout bss
hwaveout:
    resd 1

section symbols text
symbols:
    extern _su_render_song@4
    extern _CreateFileA@28
    extern _WriteFile@20
    extern _CloseHandle@4
    extern _ExitProcess@4

section entry text
    global _mainCRTStartup
_mainCRTStartup:
    push soundbuffer
    call _su_render_song@4

    push NULL
    push FILE_ATTRIBUTE_NORMAL
    push CREATE_ALWAYS
    push NULL
    push 0
    push GENERIC_WRITE
    push music_file
    call _CreateFileA@28
    mov dword [hfile], eax

    push NULL
    push bytes_written
    push SAMPLE_COUNT * CHANNEL_COUNT * SAMPLE_SIZE
    push soundbuffer
    push dword [hfile]
    call _WriteFile@20
    
    push dword [hfile]
    call _CloseHandle@4

    push 0
    call _ExitProcess@4
