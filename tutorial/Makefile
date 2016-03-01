SQ='
SRCS=$(wildcard *.Rmd)
PROD=$(SRCS:.Rmd=.html)
TUTO_DATA_DIR=/data/rethomics_tutorial_data/

all: $(PROD)

%.html: %.Rmd
	R -e "TUTO_DATA_DIR = $(SQ)$(TUTO_DATA_DIR)$(SQ);rmarkdown::render($(SQ)$<$(SQ))"

clean:
	rm -f *.html

