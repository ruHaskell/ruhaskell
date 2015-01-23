WATCH_LIST="posts/*.md src/*.hs"
CMD="
clear
cabal run build -v
echo 'BrowserReload();' | nc localhost 4242  > /dev/null
"

case "$OSTYPE" in
darwin*)
  fswatch -0 -r $WATCH_LIST | while read -d "" event
  do
    eval "$CMD"
  done
  ;;
linux*)
  while inotifywait -qq -e modify -r $WATCH_LIST
  do
    eval "$CMD"
  done
  ;;
*)
  echo "Unknown OS: $OSTYPE"
  ;;
esac
