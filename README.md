# ontrack-4k
PC-4k intro by NR4/Team210, shown in the On-Track 2023 Intro Compo (which was held on the Chaos Communication Camp 2023).

# Building
You need CMake, Ninja, clang, lld-link in your PATH. The remaining dependencies (shader_minifier, crinkler, glslangValidator, nasm, glext, khrplatform, go) can be in your system PATH, but will be downloaded automatically if they're not present. MSVC build tools or Visual Studio (specifically the Windows 10 SDK) need to be installed, but don't need to be in the system PATH if you chose default install locations.

Before building, update the git submodules:
`git submodule update --init --recursive.`

Next, create an out-of-source build directory:
`mkdir build`

Configure using
`cmake .. -G"Ninja" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=ontrack-4k -DCMAKE_TOOLCHAIN_FILE=../toolchain/lld.cmake`

and build with
`ninja`.

Build a release archive using
`ninja install`.

If you want to build the actual release archive like it's on scene.org, which may take a very long time, depending on your hardware, delete CMakeCache.txt and configure using the Crinkler toolchain:
`rm CMakeCache.txt && cmake .. -G"Ninja" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=ontrack-4k -DCMAKE_TOOLCHAIN_FILE=../toolchain/crinkler.cmake`.

Then use the above commands to build and install. The release archive will be available in the install-prefix `ontrack-4k`.

# License
This product is (c) 2023 Alexander Kraus <nr4@z10.info> and GPLv3 (see LICENSE for details).

# Credits
I'm back to writing intros. This time, I wrote music, graphics and code all by myself. Enjoy.

Greetings go out to LJ, epoqe and Team210.

I can not die.
