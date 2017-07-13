SQ='
SRCS=$(wildcard *.Rmd)
PROD=$(SRCS:.Rmd=.html)
TUTO_DATA_DIR=/data/rethomics_tutorial_data/
TUTO_MAKER=make_tutorial_data.R
TUTO_DATA_ZIP=/tmp/rethomics_tutorial_data.zip


all: $(PROD)
tuto_zip: $(TUTO_DATA_ZIP)

%.html: %.Rmd
	R -e "TUTO_DATA_DIR = $(SQ)$(TUTO_DATA_DIR)$(SQ);rmarkdown::render($(SQ)$<$(SQ))"

tuto_data: $(TUTO_MAKER)
	R -e "TUTO_DATA_DIR = $(SQ)$(TUTO_DATA_DIR)$(SQ); source($(SQ)$<$(SQ))"
	
$(TUTO_DATA_ZIP): 
	cd $(TUTO_DATA_DIR)/.. &&	zip $@ -7 -T -r rethomics_tutorial_data/

publish_data: tuto_zip
	firefox https://zenodo.org/record/826874

clean:
	rm -f *.html

