.PHONY: all
all:
	./configure
	cd output && make

.PHONY: test
test: all
	cd output && make test

