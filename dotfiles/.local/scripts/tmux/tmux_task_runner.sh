#!/usr/bin/env bash

clear
echo "Buscando tareas en: $(basename "$PWD")"

if [ -f ".tmux-env" ]; then
    source .tmux-env
fi

if [ -n "$TMUX_TASK_CUSTOM_RUNNER" ]; then
    # if [ -f "$TMUX_TASK_CUSTOM_RUNNER" ]; then
        echo "Usando runner personalizado definido en TMUX_TASK_CUSTOM_RUNNER..."
        bash $TMUX_TASK_CUSTOM_RUNNER

        EXIT_CODE=$?
        goto_end=true
    # else
    #     echo "La variable TMUX_TASK_CUSTOM_RUNNER apunta a '$TMUX_TASK_CUSTOM_RUNNER', pero el archivo no existe."
    # fi
fi

# Solo ejecuta el resto si no encontró el runner personalizado
if [ "$goto_end" != true ]; then

    if [ -f "./task_runner.sh" ]; then
        echo "Ejecutando task_runner.sh local..."
        ./task_runner.sh

    elif [ -f "package.json" ]; then
        SCRIPTS=$(jq -r '.scripts | keys[]' package.json 2>/dev/null)

        if [ -z "$SCRIPTS" ]; then
            echo "No se encontraron scripts en package.json."
        else
            SELECTED=$(echo "$SCRIPTS" | fzf --height 40% --reverse --prompt="Elige un script de NPM: ")

            if [ -n "$SELECTED" ]; then
                clear
                echo "Ejecutando: npm run $SELECTED"
                echo "------------------------------------------------"
                npm run "$SELECTED"
            else
                echo "Cancelado."
                exit 0
            fi
        fi

    elif [ -f "Makefile" ]; then
        TARGETS=$(awk -F: '/^[a-zA-Z0-9_-]+:/ {print $1}' Makefile)

        SELECTED=$(echo "$TARGETS" | fzf --height 40% --reverse --prompt="Elige un target de Make: ")

        if [ -n "$SELECTED" ]; then
            clear
            echo "Ejecutando: make $SELECTED"
            echo "------------------------------------------------"
            make "$SELECTED"
        else
            echo "Cancelado."
            exit 0
        fi

    else
        echo "No se ha detectado ningún entorno conocido."
    fi

    EXIT_CODE=$?
fi

if [ $EXIT_CODE -ne 0 ]; then
    echo ""
    echo "El proceso terminó con el código $EXIT_CODE."
    read -p "Presiona ENTER para cerrar este panel..."
fi
