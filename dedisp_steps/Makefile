CC = gcc
CFLAGS = -c -O3 -D_GNU_SOURCE -D_FILE_OFFSET_BITS=64
INCL = -I./ #-I/home/pulsar/pgplot/
LIBS = -lm

all: dedisp_steps_new 

dedisp_steps_new: dedisp_steps_new.o read_data.o strings_equal.o
	$(CC) dedisp_steps_new.o read_data.o -o dedisp_steps_new $(LIBS)

read_data.o: read_data.c
	$(CC) $(CFLAGS) $(INCL) read_data.c

strings_equal.o: strings_equal.c
	$(CC) $(CFLAGS) $(INCL) strings_equal.c

strip:
	strip dedisp_steps_new

clean:
	rm -f *~ *.o
