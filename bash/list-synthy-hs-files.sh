find . -name "*.hs" \
  | grep "Montevideo" \
  | egrep -v ".stack-work|learning|mtv-earTrain|mtv-ji|Unused" \
  > hs-files
