all: armatrix.so

armatrix.so: armatrix.cpp
	Rscript --vanilla -e "TMB::compile('armatrix.cpp','-O0 -g')"

clean:
	rm -rf *o