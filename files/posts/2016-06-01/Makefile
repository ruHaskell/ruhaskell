.PHONY: all clean

SVGS = $(wildcard *.svg)
PNGS = $(SVGS:.svg=.png)

all : ${PNGS}

clean :
	rm -f ${PNGS}

%.png : %.svg
	inkscape -z -D -e $@ -w 500 $<
	optipng $@

