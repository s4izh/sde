#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import logging
import sys
import signal
from pathlib import Path
from typing import Final, Optional

# Constants
DEFAULT_TIMEOUT: Final[int] = 30

def signal_handler(sig, frame):
    # cleanup logic
    sys.exit(0)

signal.signal(signal.SIGINT, signal_handler)

def setup_logging(level: int = logging.INFO) -> None:
    logging.basicConfig(
        level=level,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        stream=sys.stderr
    )

def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="[Module Description]",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    parser.add_argument("-v", "--verbose", action="store_true", help="Set log level to DEBUG")
    parser.add_argument("-i", "--input", type=Path, required=True, help="Input file path")

    # Posicionales
    parser.add_argument("source", help="Source file")
    parser.add_argument("dest", help="Destination file")

    # Posicionales variables (nargs)
    parser.add_argument("extra", nargs="*", help="Extra files")

    return parser.parse_args()

def main() -> None:
    args = parse_arguments()
    setup_logging(logging.DEBUG if args.verbose else logging.INFO)
    
    try:
        # execution logic
        pass
    except Exception as e:
        logging.error(f"Fatal execution error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
