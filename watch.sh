while true; do
    clear
    cabal run build
    inotifywait -qq -e modify -r posts/ src/
done
