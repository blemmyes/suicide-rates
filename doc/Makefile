.PHONY: clean

paper.pdf: paper.tex paper.toc 1-data.tex
	xelatex paper.tex && xelatex paper.tex

paper.toc:
	xelatex paper.tex

clean:
	rm -f *.aux *.log *.toc
