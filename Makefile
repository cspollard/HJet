CXX=g++

all: run Main

clean:
	rm *.o *.hi run Main

run: main.o TI.o
	$(CXX) main.o TI.o -o run `root-config --libs`

main.o: main.cxx
	$(CXX) main.cxx -c -o main.o `root-config --cflags`

TI.o: TI.C
	$(CXX) TI.C -c -o TI.o `root-config --cflags`

Main: Main.hs Jet.hs LorentzVector.hs
	ghc --make -O2 Main.hs -rtsopts
