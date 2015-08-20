SQ='
SRCS=$(wildcard *.Rmd)
PROD=$(SRCS:.Rmd=.html)

all: $(PROD) tutorial.html

%.html: %.Rmd
	R -e "rmarkdown::render($(SQ)$<$(SQ))"

tutorial.html : make_tuto.py
	python make_tuto.py

clean:
	rm -f *.html

