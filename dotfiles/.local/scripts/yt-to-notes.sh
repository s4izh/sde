#!/usr/bin/env bash

NOTES_DIR="$HOME/notes"
# URL=$(xclip -o -selection clipboard)
URL=$(wl-paste)

if [[ ! "$URL" =~ (youtube\.com|youtu\.be) ]]; then
    echo "Error: Clipboard does not contain a valid YouTube link."
    echo "Current clipboard: $URL"
    read -n 1 -s -r -p "Press any key to exit..."
    exit 1
fi

echo "Fetching metadata for: $URL"
echo "Please wait..."

TITLE=$(yt-dlp --print "%(title)s" "$URL" 2>/dev/null)
UPLOADER=$(yt-dlp --print "%(uploader)s" "$URL" 2>/dev/null)
DATE=$(yt-dlp --print "%(upload_date)s" "$URL" 2>/dev/null)

if [ -z "$TITLE" ]; then
    echo "Error: Failed to fetch metadata. Is the video private or link broken?"
    read -n 1 -s -r -p "Press any key to exit..."
    exit 1
fi

cd "$NOTES_DIR" || exit

TARGET_FILE=$(find . -type f -name "*.md" | fzf --prompt="Select Note to append to: " --info=inline --border)

if [ -n "$TARGET_FILE" ]; then
    {
        echo ""
        echo "### [$TITLE]($URL)"
        echo "- **Channel:** $UPLOADER"
        echo "- **Published:** $DATE"
        echo ""
    } >> "$TARGET_FILE"

    echo "Successfully saved to $TARGET_FILE!"
    sleep 1.5
else
    echo "Operation cancelled (no file selected)."
    sleep 1
fi
