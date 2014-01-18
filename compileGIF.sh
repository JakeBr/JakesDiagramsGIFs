# arguments:
#  1. fileroot of .hs file
#  2. number of frames
#  3. delay in 1/100th of a second (I recommend at least 5)
#  4. width of .gif file in pixels

ghc --make $1
./diagramsBatch ./$1 $2 $4 $1x
./mkGIF_no_dither $1x $2 $3 $1.gif
rm $1x*
rm $1
rm $1.o
rm $1.hi
firefox $1.gif
