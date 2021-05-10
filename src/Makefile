all: armatrix.so

armatrix.so: template1.cpp
	Rscript --vanilla -e "TMB::compile('armatrix.cpp','-O0 -g')"

clean:
	rm -rf *o