ghc --make $1
./$1 -o $1.gif -w $2
rm $1
rm $1.hi
rm $1.o
