MAIN=main
SRCS=$(wildcard *.tex *.sty *.bib)

.PHONY: all clean reallyclean
all: ${MAIN}.pdf
clean:
	latexmk -c
reallyclean:
	latexmk -C

%.pdf: %.tex ${SRCS}
	latexmk -pdf $<
