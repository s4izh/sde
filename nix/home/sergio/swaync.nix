{ pkgs, ... }:

{
  # swaync loads only one style.css, so ours is the stock one plus overrides
  xdg.configFile."swaync/style.css".text =
    builtins.readFile "${pkgs.swaynotificationcenter}/etc/xdg/swaync/style.css"
    + ''

      /* opaque notifications and control center */
      :root {
        --cc-bg: rgb(46, 46, 46);
        --noti-bg-alpha: 1;
        --noti-bg-focus: rgb(68, 68, 68);
      }
    '';
}
