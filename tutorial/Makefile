SQ='
SRCS=$(wildcard *.Rmd)
PROD=$(SRCS:.Rmd=.html)

all: $(PROD)

%.html: %.Rmd
	R -e "rmarkdown::render($(SQ)$<$(SQ))"

clean:
	rm -f *.html


