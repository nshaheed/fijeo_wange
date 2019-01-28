LILY_CMD_LINUX = lilypond -ddelete-intermediate-files -dno-point-and-click -drelative-includes

all: join

score:
	lilypond -dpoint-and-click=#f score.ly

legend:
	lilypond-book --pdf --output=legend legend.lytex
	# cd dir; pdflatex legend.tex
	# xelatex -output-directory=legend legend/legend.tex
	cd legend; xelatex legend.tex
	mv legend/legend.pdf legend.pdf

join: score legend
	pdfunite legend.pdf score.pdf Fijeo_Wange_Shaheed.pdf

clean:
	rm *.pdf || true
	rm -rf legend/ || true


