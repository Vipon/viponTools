.PHONY: all
all:
	./configure
	cd output && make

.PHONY: test
test: all
	cd output && make test

.PHONY: release
release:
	./configure --release
	cd output && make -j
	cd output && make test
	cd output && make install
	cd output && tar -czvf viponTools.tar.gz viponTools

