{ pkgs, lib, inputs, ... }:
{
  environment.systemPackages = [
    inputs.bochs-soa.packages.x86_64-linux.bochs
    inputs.bochs-soa.packages.x86_64-linux.default
    pkgs.gcc9
    pkgs.dev86
  ];
}
