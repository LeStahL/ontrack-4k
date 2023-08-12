; Those are constants that need to be searched in the 
; Windows SDK or OpenGL headers.
%define CDS_FULLSCREEN 0x4
%define PFD_DRAW_TO_WINDOW 0x4
%define PFD_TYPE_RGBA 0
%define PFD_SUPPORT_OPENGL 0x20
%define PFD_DOUBLEBUFFER 0x1
%define DM_PELSWIDTH 0x80000
%define DM_PELSHEIGHT 0x100000
%define WS_OVERLAPPED 0x00000000
%define WS_POPUP 0x80000000
%define WS_VISIBLE 0x10000000
%define WS_MAXIMIZE 0x01000000
%define ATOM_STATIC 0xc019 ; las/hg & manx/hg rule.
%define GL_FRAGMENT_SHADER 0x8B30
%define PM_REMOVE 0x1
%define VK_ESCAPE 0x1B
%define WAVE_FORMAT_IEEE_FLOAT 0x3
%define WHDR_PREPARED 0x2
%define WAVE_MAPPER 0xFFFFFFFF
%define TIME_SAMPLES 0x2

; Those we link.
section declarations text
declarations:
    extern _GetAsyncKeyState@4
    extern _ChangeDisplaySettingsA@8
    extern _CreateWindowExA@48
    extern _ShowCursor@4
    extern _GetDC@4
    extern _ChoosePixelFormat@8
    extern _SetPixelFormat@12
    extern _wglCreateContext@4
    extern _wglMakeCurrent@8
    extern _SwapBuffers@4
    extern _wglGetProcAddress@4
    extern _glRecti@16
    extern _PeekMessageA@20
    extern _DispatchMessageA@4
    extern _su_render_song@4
    extern _CreateThread@24
    extern _waveOutOpen@24
    extern _waveOutWrite@12

; Shader source, output of the minifier.
%include "gfx.inc"
shadersource:
    dd _gfx_frag

; Those we get over wglGetProcAddress.
section glcreateshaderprogramv data
glcreateshaderprogramv:
    db "glCreateShaderProgramv", 0

section gluseprogram data
gluseprogram:
    db "glUseProgram", 0

section gluniform1i data
gluniform1i:
    db "glUniform1i", 0

section glgenframebuffers data
glgenframebuffers:
    db "glGenFramebuffers", 0

section glactivetexture data
glactivetexture:
    db "glActiveTexture", 0

section glbindframebuffer data
glbindframebuffer:
    db "glBindFramebuffer", 0

section glframebuffertexture2d data
glframebuffertexture2d:
    db "glFramebufferTexture2D", 0

section gluniform2f data
gluniform2f:
    db 'glUniform2f', 0

section gluniform1f data
gluniform1f:
    db 'glUniform1f', 0

; For win32 messages.
section msg bss
msg:
    resd 1
message:
    resd 7

; Graphics related data.
section pixelformat data
pixelformat:
    dw (pixelformat_end - pixelformat)
    dw 1
    dd PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER
    db PFD_TYPE_RGBA
    db 32
    times 6 db 0
    db 8
    times 3 dw 0
    db 32
    times 4 dd 0
pixelformat_end:

section devmode data
devmode:
    times 9 dd 0
    dw (devmode_end - devmode)
    dw 0
    dd DM_PELSWIDTH | DM_PELSHEIGHT
    times 16 dd 0
width:
    dd WIDTH
height:
    dd HEIGHT
    times 2 dd 0
devmode_end:

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

section mmtime data
mmtime:
    dd TIME_SAMPLES
time:
    times 2 dd 0

section entry text
    global _WinMainCRTStartup
_WinMainCRTStartup:
    ; Change to full screen.
    push CDS_FULLSCREEN
    push devmode
    call _ChangeDisplaySettingsA@8

    ; hwnd is in eax.
    times 8 push 0
    push WS_OVERLAPPED | WS_POPUP | WS_VISIBLE | WS_MAXIMIZE
    push 0
    push ATOM_STATIC
    push 0
    call _CreateWindowExA@48

    ; store HDC in ebp.
    push eax
    call _GetDC@4
    mov ebp, eax

    ; store chosen pixel format in eax
    push pixelformat
    push ebp
    call _ChoosePixelFormat@8

    ; Set pixel format from eax 
    push pixelformat
    push eax
    push ebp
	call _SetPixelFormat@12

    ; Create OpenGL context, store in eax
    push ebp
    call _wglCreateContext@4

    ; Make OpenGL context current.
    push eax
    push ebp
    call _wglMakeCurrent@8

    ; Swap buffers for black screen.
    push ebp
    call _SwapBuffers@4
    
    ; Create shader program, push result
    push glcreateshaderprogramv
    call _wglGetProcAddress@4

    push shadersource
    push 1
    push GL_FRAGMENT_SHADER
    call eax
    push eax

    ; Use shader program
    push gluseprogram
    call _wglGetProcAddress@4
    call eax

    ; Hide cursor.
    push 0
    call _ShowCursor@4

    ; CreateThread(0, 0, (LPTHREAD_START_ROUTINE)_4klang_render, lpSoundBuffer, 0, 0);
    times 2 push 0
    push soundbuffer
    push _su_render_song@4
    times 2 push 0
    call _CreateThread@24

    ; waveOutOpen(&hWaveOut, WAVE_MAPPER, &WaveFMT, NULL, 0, CALLBACK_NULL );
    times 3 push 0
    push wavefmt
    push WAVE_MAPPER
    push hwaveout
    call _waveOutOpen@24

    ; waveOutWrite(hWaveOut, &WaveHDR, sizeof(WaveHDR));
    push wavehdr_end - wavehdr
    push wavehdr
    push dword [hwaveout]
    call _waveOutWrite@12

    mainloop:
        ; Dispatch all available win32 messages 
        dispatchloop:
            push PM_REMOVE
            times 3 push 0
            push msg
            call _PeekMessageA@20
            cmp eax, 0
            je dispatchloop_end
            
            push msg
            call _DispatchMessageA@4

            jmp dispatchloop
        dispatchloop_end:

    ; Increment frame counter
    inc edi
    
    ; Stall until escape.
    push VK_ESCAPE
    call _GetAsyncKeyState@4

    cmp eax, 0
    je mainloop

    hlt