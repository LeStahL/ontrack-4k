; Those are constants we choose in the gfx.
%define UNIFORM_LOCATION_IFRAME 0
%define UNIFORM_LOCATION_ISAMPLE 1
%define UNIFORM_LOCATION_IPASS 2
%define UNIFORM_LOCATION_IRESOLUTION 3
%define UNIFORM_LOCATION_ICHANNEL0 4

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
 ; las/hg & manx/hg rule.
%define ATOM_STATIC 0xc019
%define GL_FRAGMENT_SHADER 0x8B30
%define PM_REMOVE 0x1
%define VK_ESCAPE 0x1B
%define WAVE_FORMAT_IEEE_FLOAT 0x3
%define WHDR_PREPARED 0x2
%define WAVE_MAPPER 0xFFFFFFFF
%define TIME_SAMPLES 0x2
%define GL_COLOR_ATTACHMENT0 0x8CE0
%define GL_TEXTURE0 0x84C0
%define GL_TEXTURE_MAG_FILTER 0x2800
%define GL_TEXTURE_MIN_FILTER 0x2801
%define GL_CLAMP 0x2900
%define GL_TEXTURE_WRAP_S 0x2802
%define GL_TEXTURE_WRAP_T 0x2803
%define GL_TEXTURE_2D 0x0DE1
%define GL_NEAREST 0x2600
%define GL_REPEAT 0x2901
%define GL_LINEAR 0x2601
%define GL_FLOAT 0x1406
%define GL_RGBA32F 0x8814
%define GL_RGBA 0x1908
%define GL_FRAMEBUFFER 0x8D40

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
    extern _waveOutGetPosition@12
    extern _glGenTextures@8
    extern _glDrawBuffer@4
    extern _glBindTexture@8
    extern _glTexImage2D@36
    extern _glTexParameteri@12

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

section gluniform2i data
gluniform2i:
    db 'glUniform2i', 0

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

; For the time.
section mmtime data
mmtime:
    dd TIME_SAMPLES
time:
    times 2 dd 0

; OpenGL stuff
section gldata data
texture:
    dd 0
framebuffer:
    dd 0

section entry text
    global _WinMainCRTStartup
_WinMainCRTStartup:
    ; CreateThread(0, 0, (LPTHREAD_START_ROUTINE)_4klang_render, lpSoundBuffer, 0, 0);
    times 2 push 0
    push soundbuffer
    push _su_render_song@4
    times 2 push 0
    call _CreateThread@24

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

    ; glGenTextures(1, &texture);
    push texture
    push 1
    call _glGenTextures@8

    ; ((PFNGLGENFRAMEBUFFERSPROC)wglGetProcAddress("glGenFramebuffers"))(1, &framebuffer);
    push glgenframebuffers
    call _wglGetProcAddress@4
    
    push framebuffer
    push 1
    call eax

	; glDrawBuffer(GL_COLOR_ATTACHMENT0);
    push GL_COLOR_ATTACHMENT0
    call _glDrawBuffer@4

	; ((PFNGLACTIVETEXTUREPROC)wglGetProcAddress("glActiveTexture"))(GL_TEXTURE0);
    push glactivetexture
    call _wglGetProcAddress@4

    push GL_TEXTURE0
    call eax

	; glBindTexture(GL_TEXTURE_2D, texture);
    push dword [texture]
    push GL_TEXTURE_2D
    call _glBindTexture@8

	; glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    push GL_LINEAR
    push GL_TEXTURE_MAG_FILTER
    push GL_TEXTURE_2D
    call _glTexParameteri@12

	; glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    push GL_LINEAR
    push GL_TEXTURE_MIN_FILTER
    push GL_TEXTURE_2D
    call _glTexParameteri@12

	; glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    push GL_REPEAT
    push GL_TEXTURE_WRAP_S
    push GL_TEXTURE_2D
    call _glTexParameteri@12
    
	; glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    push GL_REPEAT
    push GL_TEXTURE_WRAP_T
    push GL_TEXTURE_2D
    call _glTexParameteri@12

	; glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F, WIDTH, HEIGHT, 0, GL_RGBA, GL_FLOAT, 0);
    push 0
    push GL_FLOAT
    push GL_RGBA
    push 0
    push HEIGHT
    push WIDTH
    push GL_RGBA32F
    push 0
    push GL_TEXTURE_2D
    call _glTexImage2D@36

    ; ((PFNGLBINDFRAMEBUFFERPROC)wglGetProcAddress("glBindFramebuffer"))(GL_FRAMEBUFFER, framebuffers[0]);
    push glbindframebuffer
    call _wglGetProcAddress@4

    push dword [framebuffer]
    push GL_FRAMEBUFFER
    call eax

	; ((PFNGLFRAMEBUFFERTEXTURE2DPROC)wglGetProcAddress("glFramebufferTexture2D"))(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, textures[0], 0);
    push glframebuffertexture2d
    call _wglGetProcAddress@4

    push 0
    push dword [texture]
    push GL_TEXTURE_2D
    push GL_COLOR_ATTACHMENT0
    push GL_FRAMEBUFFER
    call eax

    ; Set channel uniform.
    push gluniform1i
    call _wglGetProcAddress@4
    push 0
    push UNIFORM_LOCATION_ICHANNEL0
    call eax

    ; Set resolution uniform to buffer resolution
    push gluniform2i
    call _wglGetProcAddress@4
    push dword [height]
    push dword [width]
    push UNIFORM_LOCATION_IRESOLUTION
    call eax

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

    ; That's the frame counter.
    mov edi, 0
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

        ; Set frame uniform to frame
        push gluniform1i
        call _wglGetProcAddress@4
        push edi
        push UNIFORM_LOCATION_IFRAME
        call eax

        ; Update time from wave position.
        ; waveOutGetPosition(hWaveOut, &MMTime, sizeof(MMTIME));
        push 12
        push mmtime
        push dword [hwaveout]
        call _waveOutGetPosition@12

        ; Set sample uniform to time
        push gluniform1i
        call _wglGetProcAddress@4
        push dword [time]
        push UNIFORM_LOCATION_ISAMPLE
        call eax

        ; ((PFNGLBINDFRAMEBUFFERPROC)wglGetProcAddress("glBindFramebuffer"))(GL_FRAMEBUFFER, framebuffers[i]);
        push glbindframebuffer
        call _wglGetProcAddress@4
        push dword [framebuffer]
        push GL_FRAMEBUFFER
        call eax

        ; Set pass uniform to pass
        push gluniform1i
        call _wglGetProcAddress@4
        push 0
        push UNIFORM_LOCATION_IPASS
        call eax

        ; glRecti(-1, -1, 1, 1);
        times 2 push dword 1
        times 2 push dword -1
        call _glRecti@16

        ; ((PFNGLBINDFRAMEBUFFERPROC)wglGetProcAddress("glBindFramebuffer"))(GL_FRAMEBUFFER, 0);
        push glbindframebuffer
        call _wglGetProcAddress@4
        push 0
        push GL_FRAMEBUFFER
        call eax

        ; Set pass uniform to pass
        push gluniform1i
        call _wglGetProcAddress@4
        push 1
        push UNIFORM_LOCATION_IPASS
        call eax

        ; glRecti(-1, -1, 1, 1);
        times 2 push dword 1
        times 2 push dword -1
        call _glRecti@16

        ; SwapBuffers(hDC);
        push ebp
        call _SwapBuffers@4

        ; Increment frame counter
        inc edi
        
        ; Stall until escape.
        push VK_ESCAPE
        call _GetAsyncKeyState@4

        cmp eax, 0
        je mainloop

    hlt
