.PHONY: build
build:
	eask install
	eask recompile

.PHONY: check
check:
	eask lint org README.org
	eask lint license
	eask test melpazoid
