#!/usr/bin/env python3

import subprocess
import sys
import argparse
import shlex

LAYOUT_MAP = {
    "S":  " Scroller",
    "T":  " Tile",
    "G":  " Grid",
    "M":  " Monocle",
    "K":  " Deck",
    "VS": " V-Scroller",
    "VT": " V-Tile",
    "VG": " V-Grid",
    "VK": " V-Deck",
    "CT": " Center Tile",
    "RT": " Right Tile",
    "TG": " Three Column",
}

def debug_log(message, enabled):
    if enabled:
        print(f"[DEBUG] {message}", file=sys.stderr)

def map_layout(code):
    return LAYOUT_MAP.get(code, f"{code}")

def get_current_layout(monitor, debug):
    try:
        cmd = f"mmsg -g -l -o {monitor}"
        debug_log(f"Running: {cmd}", debug)
        result = subprocess.check_output(shlex.split(cmd), text=True).strip()
        debug_log(f"Initial raw output: '{result}'", debug)
        
        # Based on your previous error, if output is "DP-3 layout T", take last element
        parts = result.split()
        if not parts:
            return "?"
        return parts[-1]
    except Exception as e:
        debug_log(f"Error getting initial layout: {e}", debug)
        return "?"

def main():
    parser = argparse.ArgumentParser(description="MangoWC Layout Mapper for Waybar")
    parser.add_argument("monitor", help="The monitor name (e.g. DP-3, eDP-1)")
    parser.add_argument("-d", "--debug", action="store_true", help="Enable debug output to stderr")
    args = parser.parse_args()

    debug_log(f"Monitoring monitor...", args.monitor)

    # 1. Print initial state
    initial_code = get_current_layout(args.monitor, args.debug)
    print(map_layout(initial_code), flush=True)

    # 2. Watch for changes
    debug_log(f"Starting watch mode: mmsg -w -l", args.debug)
    process = subprocess.Popen(
        ["mmsg", "-w", "-l"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )

    try:
        for line in iter(process.stdout.readline, ""):
            line = line.strip()
            if not line:
                continue
            
            debug_log(f"Stream line: '{line}'", args.debug)
            parts = line.split()
            
            # Match formats: ["DP-3", "layout", "T"] or ["DP-3", "T"]
            if len(parts) >= 2:
                mon = parts[0]
                code = parts[-1]
                
                if mon == args.monitor:
                    debug_log(f"Match found for {mon}! Code: {code}", args.debug)
                    print(map_layout(code), flush=True)
                else:
                    debug_log(f"Ignoring update for monitor {mon}", args.debug)
                    
    except KeyboardInterrupt:
        process.terminate()
    except Exception as e:
        debug_log(f"Runtime error: {e}", args.debug)
        sys.exit(1)

if __name__ == "__main__":
    main()
