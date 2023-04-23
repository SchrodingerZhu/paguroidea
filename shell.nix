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

