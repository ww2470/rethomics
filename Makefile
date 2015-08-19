
PACKAGE_NAME := rethomics

R_DIR = $(shell pwd)/$(PACKAGE_NAME)
R_SOURCES := $(shell ls $(R_DIR)/R/*.R )
N = $(shell cat $(R_DIR)/DESCRIPTION |grep ^Package | cut --fields=2 --delimiter=: |sed s/\ //g)
V := $(shell cat $(R_DIR)/DESCRIPTION |grep ^Version  | cut --fields=2 --delimiter=: |sed s/\ //g)
R_TGZ := $(N)_$(V).tar.gz
R_PDF := $(N).pdf
vpath %.R  $(R_DIR)


.PHONY:  R 
#I~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all : R tuto check

clean:
	rm -fr  *.tar.gz *.out *.pdf  *.log  $(R_dir)/man $(R_dir)/NAMESPACE
	$(MAKE) -C ./tutorial clean

#I~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


R : $(R_TGZ) $(R_PDF)
	@echo "installing Package"
	R CMD INSTALL --no-multiarch --with-keep.source  $(PACKAGE_NAME)
#I~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

$(R_PDF) : $(R_SOURCES)
	R CMD Rd2pdf --force  $(PACKAGE_NAME)
#I~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$(R_TGZ) : $(R_SOURCES)
	@echo "Roxygenising:"
	@echo $(R_SOURCES)
	@echo "library(roxygen2); roxygenise(package.dir=\"$(R_DIR)\") ; print(\"OK\")" | R --vanilla
	@echo "Building Package $(R_TGZ):"
	@R CMD build $(PACKAGE_NAME)
#I~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tuto: R
	$(MAKE) -C ./tutorial all
#I~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check: R
	R -e "devtools::check('rethomics')"
