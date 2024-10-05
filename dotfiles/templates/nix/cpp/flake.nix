{
  description = "Lagos Engine development environment";

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
  };
  outputs = { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in {
      devShells.${system}.default = pkgs.stdenvNoCC.mkDerivation {
        name = "lagos";
        buildInputs = with pkgs; [
          gcc13
          gdb

          # llvmPackages_14.clang
          # cmake
          # cmakeCurses
          # Development time dependencies
          # gtest

          # Build time and Run time dependencies
          # spdlog
          # abseil-cpp

          # Vulkan dependencies
          vulkan-tools
          vulkan-loader
          vulkan-headers
          vulkan-validation-layers
          spirv-tools
          xorg.libXxf86vm
          xorg.libXi
          xorg.libX11
          xorg.libXrandr

          # GLFW
          glfw
          # GLM
          glm
          # Shader compiler
          shaderc
        ];
      };
    };
}
