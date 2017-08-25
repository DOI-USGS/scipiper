# exploring cache management:

# Basic approach:
# Every data file is represented by an indicator file. Indicator files get created once we believe a cached version exists (we just posted it, we checked for it, or both). 
# Downstream files depend on the indicator files rather than the data files themselves. Before running any data processing scripts, the individual targets are pulled from the cache as need. Multiple files can be pulled in a single make call within the recipe, e.g., `make B.rds A.txt`
# The official targets of data analysis recipes are indicator files, not data files. Those recipes are responsible for ensuring that output data files do exist on the cache (and optionally also locally).

# A.txt is a raw, collaborator-provided data file. It's either in the cache already or it's not. If it is, make a corresponding status indicator file with matching timestamp. If not, don't make the indicator file. Commit the indicator file so everybody has it. (Our example 'cache' is just files with a '.cache' suffix)
# [ -e x ] tests whether file x exists
# $(shell date -r ...) gets the timestamp of a file
# > writes the text on the left to a file on the right
# touch -d changes the timestamp of a file to match a text date
# the sequence from if to fi needs to be a single make line, hence the \'s at the ends of lines
A.txt.st:
	$(if $(wildcard $(subst .st,.cache,$@)),\
	echo "found $(subst .st,,$@) in cache";\
		DT="$(shell date -u '+%Y-%m-%d %H:%M:%S %z' -r $(subst .st,.cache,$@))";\
		echo $$DT > $@;\
		touch -d "$$DT" $@;,\
	$(error $(subst .st,,$@) is not in cache);)

# B.rds is the result of processing A.txt. Its prerequisite is A.txt.st, which everybody should have, and it only checks for A.txt if it's decided that B.rds.st actually needs to run. After it is run, the only Git-committed product is the status indicator file B.rds.st
B.rds.st : A.txt.st
	@echo "making B.rds.st, B.rds, and B.rds.cache"
	make A.txt
	@cp A.txt B.rds
	@cp B.rds B.rds.cache
	@touch $@
	@touch B.rds

# B.rds should be available in the cache iff B.rds.st exists, and B.rds is out of date if it's older than B.rds.st. making B.rds actually means either (1) pulling B.rds from the cache or (2) making B.rds.st. If we call `make B.rds` and #2 applies, then it's worth a timestamp check after the dependencies are built, because B.rds probably already exists locally as a side effect of building B.rds.st, but `make` won't have noticed for us so we need to check ourselves.
	
# [ x -ot y ] tests whether x is older than y to within ~1 second
% : %.st
	@if [ $@ -ot $@.st -o ! -e $@ ]; then\
		echo "downloading $@";\
		cp $@.cache $@;\
		touch -d "$(file $@.st)" $@;\
	else\
		echo "$@ is available locally";\
	fi


# scenarios

ls :
	ls -l A.txt B.rds.st B.rds.cache B.rds

a1 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 08:00:00 -0700" A.txt.cache
	touch -d "2017-08-25 08:00:00 -0700" A.txt.st
	@echo "make B.rds should make B+C+D, pass over C"
a2 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 08:00:00 -0700" A.txt
	touch -d "2017-08-25 10:00:00 -0700" B.rds
	@echo "make B.rds should make B+C+D, pass over C"
a3 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 08:00:00 -0700" A.txt
	touch -d "2017-08-25 09:00:00 -0700" B.rds.st
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	@echo "make B.rds should pull C from D"
a4 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 08:00:00 -0700" A.txt
	touch -d "2017-08-25 09:00:00 -0700" B.rds.st
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	touch -d "2017-08-25 07:00:00 -0700" B.rds
	@echo "make B.rds should pull C from D"
a5 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 08:00:00 -0700" A.txt
	touch -d "2017-08-25 09:00:00 -0700" B.rds.st
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	touch -d "2017-08-25 10:00:00 -0700" B.rds
	@echo "make B.rds should do nothing"
b1 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 11:00:00 -0700" A.txt
	touch -d "2017-08-25 09:00:00 -0700" B.rds.st
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	@echo "make B.rds should make B+C+D, pass over C"
b2 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 11:00:00 -0700" A.txt
	touch -d "2017-08-25 09:00:00 -0700" B.rds.st
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	touch -d "2017-08-25 10:00:00 -0700" B.rds
	@echo "make B.rds should make B+C+D, pass over C"
b3 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 11:00:00 -0700" A.txt
	touch -d "2017-08-25 10:00:00 -0700" B.rds
	@echo "make B.rds should make B+C+D, pass over C"
c1 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 10:00:00 -0700" B.rds
	@echo "make B.rds should give error about missing A"
c2 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 09:00:00 -0700" B.rds.st
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	@echo "make B.rds should pull C from D"
c3 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 09:00:00 -0700" B.rds.st
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	touch -d "2017-08-25 07:00:00 -0700" B.rds
	@echo "make B.rds should pull C from D"
c4 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 09:00:00 -0700" B.rds.st
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	touch -d "2017-08-25 10:00:00 -0700" B.rds
	@echo "make B.rds should do nothing"

clean:
	rm -f A.txt* B.rds*
