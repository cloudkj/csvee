csvee:
	csc -c parse.scm
	csc csvee.scm parse.o -o csvee

clean:
	rm -rf csvee *.o

test:
	csi -q -b parse_test.scm
