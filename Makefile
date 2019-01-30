.PHONY: all
all: build

.PHONY: depend depends
depend depends:
	dune external-lib-deps --missing @install @runtest

.PHONY: build
build: depends
	dune build @install

.PHONY: test
test: depends
	dune runtest -j 1 --no-buffer -p owl

.PHONY: clean
clean:
	dune clean

.PHONY: install
install: build
	dune install

.PHONY: uninstall
uninstall:
	dune uninstall

.PHONY: doc
doc:
	dune build @doc

.PHONY: cleanall
cleanall:
	dune uninstall && dune clean
	$(RM) -r $(find . -name .merlin)

.PHONY: push
push:
	git commit -am "coding ..." && \
	git push origin `git branch | grep \* | cut -d ' ' -f2`

.PHONY: images
images: image_lwae

image_lwae: image_owl
	docker image build . --network host -f docker/Dockerfile -t lwae

image_owl:
	docker image build -t owl --network host https://raw.githubusercontent.com/owlbarn/owl/master/docker/Dockerfile.ubuntu
