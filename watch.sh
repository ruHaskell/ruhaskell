while inotifywait -qq -e modify -r posts/ src/
do
    clear
    cabal run build
    echo "BrowserReload();" | nc localhost 4242  > /dev/null
done
