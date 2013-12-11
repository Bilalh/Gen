CFLAGS?=-Wall -g 
TARGETS=timeout5

timeout5: timeout5.o
	$(CC) $(CFLAGS) $^ -o $@

clean:
	rm -f *~ *.o $(TARGETS)
