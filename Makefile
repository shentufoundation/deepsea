NAME=language-reference

pdf:
	make tex
	make clean

tex:
	pandoc -s -o ${NAME}.tex ${NAME}.md
	gsed -i 's/.*newenvironment{Shaded}.*/ \
		\\newenvironment{Shaded}{\\small}{\\normalsize}/' \
		${NAME}.tex
	gsed -i '/.*hypertarget{introduction}.*/i \
		\\\tableofcontents' \
		${NAME}.tex
	pdflatex ${NAME}.tex
	pdflatex ${NAME}.tex

clean:
	rm -f *.aux *.fdb_latexmk *.fls *.log *.toc *.tex

.PHONY: tex pdf clean 
