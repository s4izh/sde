#!/usr/bin/env bash

NOTES_DIR="$HOME/notes"
URL=$(wl-paste)

if [[ ! "$URL" =~ ^https?:// ]]; then
    echo "Error: Clipboard does not contain a valid URL."
    echo "Current clipboard: $URL"
    read -n 1 -s -r -p "Press any key to exit..."
    exit 1
fi

echo "Processing URL: $URL"

get_youtube_metadata() {
    local url="$1"
    echo "Fetching YouTube metadata via yt-dlp..." >&2

    local title uploader date
    title=$(yt-dlp --print "%(title)s" "$url" 2>/dev/null)
    uploader=$(yt-dlp --print "%(uploader)s" "$url" 2>/dev/null)
    date=$(yt-dlp --print "%(upload_date)s" "$url" 2>/dev/null)

    if [ -z "$title" ]; then
        echo "Error: Failed to fetch YouTube metadata." >&2
        exit 1
    fi

    cat <<EOF
### [$title]($url)
- **Channel:** $uploader
- **Published:** $date
EOF
}

get_generic_metadata() {
    local url="$1"
    echo "Fetching generic webpage title..." >&2

    local title
    title=$(curl -sL --max-time 5 "$url" | grep -iPo '(?<=<title>)[^<]*(?=</title>)' | head -n 1 | xargs)

    if [ -z "$title" ]; then
        title="Web Link"
    fi

    cat <<EOF
### [$title]($url)
EOF
}

FORMATTED_DATA=""

case "$URL" in
    *youtube.com*|*youtu.be*)
        FORMATTED_DATA=$(get_youtube_metadata "$URL")
        ;;
    *)
        FORMATTED_DATA=$(get_generic_metadata "$URL")
        ;;
esac

if [ -z "$FORMATTED_DATA" ]; then
    echo "Failed to generate metadata."
    read -n 1 -s -r -p "Press any key to exit..."
    exit 1
fi


cd "$NOTES_DIR" || exit

TARGET_FILE=$(find . -type f -name "*.md" | fzf --prompt="Select Note to append to: " --info=inline --border)

if [ -n "$TARGET_FILE" ]; then
    {
        echo ""
        echo "$FORMATTED_DATA"
        echo ""
    } >> "$TARGET_FILE"

    LINE_COUNT=$(wc -l < "$TARGET_FILE")
    TARGET_LINE=$((LINE_COUNT))

    echo "Successfully saved to $TARGET_FILE!"

    nvim "+$TARGET_LINE" "$TARGET_FILE"
else
    echo "Operation cancelled (no file selected)."
fi
