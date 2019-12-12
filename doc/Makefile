.PHONY: clean

paper.pdf: paper.tex paper.toc 0-title.tex 1-research-question.tex 2-data.tex
	xelatex paper.tex && xelatex paper.tex

paper.toc:
	xelatex paper.tex

clean:
	rm -f *.aux *.log *.toc
