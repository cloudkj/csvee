csvee:
	csc csvee.scm

clean:
	rm -rf csvee

test:
	csi -q -b parse_test.scm
