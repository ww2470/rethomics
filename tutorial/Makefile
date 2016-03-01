SQ='
SRCS=$(wildcard *.Rmd)
PROD=$(SRCS:.Rmd=.html)

all: $(PROD) tutorial.html

%.html: %.Rmd
	R -e "rmarkdown::render($(SQ)$<$(SQ))"

clean:
	rm -f *.html

