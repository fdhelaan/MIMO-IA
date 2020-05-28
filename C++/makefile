CC=g++
CFLAGS=-lncurses -lpthread

parallel: parallel.o
	$(CC) -o parallel parallel.o $(CFLAGS)

parallel.o: parallel.cpp
	$(CC) -c parallel.cpp $(CFLAGS)

clean:
	rm *.o parallel
