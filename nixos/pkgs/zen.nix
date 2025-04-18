# zen-browser.nix
{
  stdenv,
  lib,
  fetchTarball,
  makeWrapper,
  copyDesktopItems,
  wrapGAppsHook,
  # Below are the runtime libraries needed
  alsa-lib,
  atk,
  cairo,
  cups,
  dbus,
  ffmpeg,
  fontconfig,
  freetype,
  gdk-pixbuf,
  glib,
  gtk3,
  libGL,
  libGLU,
  libX11,
  libXScrnSaver,
  libXcomposite,
  libXcursor,
  libXdamage,
  libXext,
  libXfixes,
  libXi,
  libXrandr,
  libevent,
  libffi,
  libglvnd,
  libjpeg,
  libnotify,
  libpng,
  libpulseaudio,
  libstartup_notification,
  libva,
  libvpx,
  libwebp,
  libxcb,
  libxkbcommon,
  libxml2,
  mesa,
  pango,
  pciutils,
  pipewire,
  udev,
  xcb-util-cursor,
  zlib,
  system,
  # This needs to be passed explicitly
  desktopFile,
}:

let
  version = "1.11.4b"; # Keep version defined here

  downloadData = {
    url = "https://github.com/zen-browser/desktop/releases/download/${version}/zen.linux-x86_64.tar.xz";
    sha256 = "sha256:0yi70x3hb4jkprxqx697gssfpz4bfrhy2gb6zbcfwlh3vc53spmq";
  };

  runtimeLibs = [
    libGL
    libGLU
    libevent
    libffi
    libjpeg
    libpng
    libstartup_notification
    libvpx
    libwebp
    stdenv.cc.cc
    fontconfig
    libxkbcommon
    zlib
    freetype
    gtk3
    libxml2
    dbus
    xcb-util-cursor
    alsa-lib
    libpulseaudio
    pango
    atk
    cairo
    gdk-pixbuf
    glib
    udev
    libva
    mesa
    libnotify
    cups
    pciutils
    ffmpeg
    libglvnd
    pipewire
    # Xorg libs explicitly listed as dependencies
    libxcb
    libX11
    libXcursor
    libXrandr
    libXi
    libXext
    libXcomposite
    libXdamage
    libXfixes
    libXScrnSaver
  ];

in
stdenv.mkDerivation {
  inherit version;
  pname = "zen-browser"; # Include variant in name

  src = fetchTarball {
    url = downloadData.url;
    sha256 = downloadData.sha256;
  };

  # desktopSrc = ./.; # This is no longer needed, use desktopFile arg

  nativeBuildInputs = [
    makeWrapper
    copyDesktopItems
    wrapGAppsHook
  ];

  # Note: No separate buildPhase needed as we are using pre-built binaries

  phases = [
    "installPhase"
    "fixupPhase"
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin

    # Use --strip-components=1 if the tarball has a top-level directory
    # If not, adjust the cp command accordingly. Assuming no top-level dir:
    cp -r $src/* $out/bin

    install -Dm755 $src/zen $out/bin/zen
    install -Dm755 $src/zen-bin $out/bin/zen-bin
    install -Dm755 $src/glxtest $out/bin/glxtest
    install -Dm755 $src/updater $out/bin/updater
    install -Dm755 $src/vaapitest $out/bin/vaapitest

    # Install the desktop file passed as an argument
    install -Dm644 ${desktopFile} $out/share/applications/zen.desktop

    # Install the icon
    install -Dm644 $out/bin/browser/chrome/icons/default/default128.png $out/share/icons/hicolor/128x128/apps/zen.png

    runHook postInstall
  '';

  fixupPhase = ''
    runHook preFixup

    # Make binaries executable
    chmod +x $out/bin/*

    # Common library path
    local libPath="${lib.makeLibraryPath runtimeLibs}"

    # Get the dynamic linker from stdenv
    local dynamicLinker=$(cat $NIX_CC/nix-support/dynamic-linker)

    # Patch and wrap main binaries
    patchelf --set-interpreter "$dynamicLinker" $out/bin/zen
    wrapProgram $out/bin/zen \
      --set LD_LIBRARY_PATH "$libPath" \
      --set MOZ_LEGACY_PROFILES 1 \
      --set MOZ_ALLOW_DOWNGRADE 1 \
      --set MOZ_APP_LAUNCHER zen \
      --prefix XDG_DATA_DIRS : "$GSETTINGS_SCHEMAS_PATH" # GSETTINGS_SCHEMAS_PATH comes from wrapGAppsHook

    patchelf --set-interpreter "$dynamicLinker" $out/bin/zen-bin
    wrapProgram $out/bin/zen-bin \
      --set LD_LIBRARY_PATH "$libPath" \
      --set MOZ_LEGACY_PROFILES 1 \
      --set MOZ_ALLOW_DOWNGRADE 1 \
      --set MOZ_APP_LAUNCHER zen \
      --prefix XDG_DATA_DIRS : "$GSETTINGS_SCHEMAS_PATH"

    # Patch and wrap helper binaries
    patchelf --set-interpreter "$dynamicLinker" $out/bin/glxtest
    wrapProgram $out/bin/glxtest --set LD_LIBRARY_PATH "$libPath"

    patchelf --set-interpreter "$dynamicLinker" $out/bin/updater
    wrapProgram $out/bin/updater --set LD_LIBRARY_PATH "$libPath"

    patchelf --set-interpreter "$dynamicLinker" $out/bin/vaapitest
    wrapProgram $out/bin/vaapitest --set LD_LIBRARY_PATH "$libPath"

    runHook postFixup
  '';

  meta = {
    description = "Zen Browser";
    homepage = "https://github.com/zen-browser/desktop";
    license = lib.licenses.mpl20; # Assuming MPL 2.0 like Firefox
    platforms = [ "x86_64-linux" ];
    mainProgram = "zen";
  };
}
