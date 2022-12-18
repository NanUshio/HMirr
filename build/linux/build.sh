cd ../../source
ghc -O3 main.hs
rm *.hi 
rm *.o  
mv main ../bin/HMirr
cd ../build/linux
cp ../../bin/HMirr ./