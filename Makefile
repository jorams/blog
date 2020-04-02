.PHONY: all web clean

all:
	sbcl --noinform --disable-debugger --load generate.lisp --quit

web: all
	python -m http.server -d _site

clean:
	rm -rf _site
