CFLAGS = -O

all: ymake-filter

ymake-filter: ymake-filter.o
	$(CC) $(CFLAGS) -o ymake-filter ymake-filter.o

clean:
	@-rm -f ymake-filter *.o core a.out
