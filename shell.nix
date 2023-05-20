/* Copyright (c) 2023 Paguroidea Developpers
 *
 * Licensed under the Apache License, Version 2.0
 * <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
 * license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
 * option. All files in the project carrying such notice may not be copied,
 * modified, or distributed except according to those terms.
 */

{ pkgs ? import <nixpkgs> {} }:
  pkgs.gcc.stdenv.mkDerivation {
    name = "paguroidea";
    buildInputs = with pkgs; [
        openssl
        pkg-config
        cmake
        gcc
        autoconf
        automake
        ninja
        gnumake
        zlib
        llvmPackages_latest.clang
        llvmPackages_latest.libclang
        llvmPackages_latest.libclang.lib
        llvmPackages_latest.llvm
        llvmPackages_latest.lld
    ];
    shellHook = ''
        export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.llvmPackages_latest.libclang.lib}/lib
        export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.stdenv.cc.cc.lib}/lib
        export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.zlib}/lib
    '';
}

