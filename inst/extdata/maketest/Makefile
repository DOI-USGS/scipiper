# exploring cache management:

# Basic approach:
# Every data file is represented by an indicator file. Indicator files get created once we believe a cached version exists (we just posted it, we checked for it, or both). 
# Downstream files depend on the indicator files rather than the data files themselves. Before running any data processing scripts, the individual targets are pulled from the cache as need. Multiple files can be pulled in a single make call within the recipe, e.g., `make B.rds A.txt`
# The official targets of data analysis recipes are indicator files, not data files. Those recipes are responsible for ensuring that output data files do exist on the cache (and optionally also locally).

# this macro creates a timestamp text string in UTC
DATETIME=echo "$(shell date -u '+%Y-%m-%d %H:%M:%S %z')"

# A.txt is a raw, collaborator-provided data file. It's either in the cache already or it's not. If it is, make a corresponding status indicator file with matching timestamp. If not, don't make the indicator file. Commit the indicator file so everybody has it.
A.txt.st :
	@make -s A.txt.stfromcache

# Here's the generic target for making a status indicator file based on the presence & timestamp of a corresponding file in the cache. Used by A.txt.st, and could be used by B.rds.st if we didn't trust S3 to receive our transmission reliably
# (Our example 'cache' is just files with a '.cache' suffix)
# [ -e x ] tests whether file x exists
# $(shell date -r ...) gets the timestamp of a file
# > writes the text on the left to a file on the right
# touch -d changes the timestamp of a file to match a text date
# the sequence from if to fi needs to be a single make line, hence the \'s at the ends of lines
%.stfromcache :
	@$(if $(wildcard $(subst .stfromcache,.cache,$@)),\
	echo "made $(subst .stfromcache,.st,$@) because found $(subst .stfromcache,,$@) in cache";\
		DT="$(shell date -u '+%Y-%m-%d %H:%M:%S %z' -r $(subst .stfromcache,.cache,$@))";\
		echo $$DT > $(subst .stfromcache,.st,$@);\
		touch -d "$$DT" $(subst .stfromcache,.st,$@);,\
	$(error $(subst .stfromcache,,$@) is not in cache);)

# make an .st file based on confidence that a corresponding cache file exists and was created at a similar time. This is reasonable if our cache is local or if checking remotely is more onerous than the likelihood*consequence of occasionally being wrong
%.stbytrust :
	@echo "made $(subst .stbytrust,.st,$@) on trust that $(subst .stbytrust,,$@) is in cache"
	@${DATETIME} > $(subst .stbytrust,.st,$@)

# B.rds is the result of processing A.txt. Its prerequisite is A.txt.st, which everybody should have, and it only checks for A.txt if it's decided that B.rds.st actually needs to run. After it is run, the only Git-committed product is the status indicator file B.rds.st
B.rds.st : A.txt.st
	@make -s A.txt
	@echo "making B.rds and B.rds.cache"
	@cp A.txt B.rds
	@cp B.rds B.rds.cache
	@make -s $@bytrust
	@touch B.rds

# B.rds should be available in the cache iff B.rds.st exists, and B.rds is out of date if it's older than B.rds.st. making B.rds actually means either (1) pulling B.rds from the cache or (2) making B.rds.st. If we call `make B.rds` and #2 applies, then it's worth a timestamp check after the dependencies are built, because B.rds probably already exists locally as a side effect of building B.rds.st, but `make` won't have noticed for us so we need to check ourselves.
	
# [ x -ot y ] tests whether x is older than y to within ~1 second
% : %.st
	@$(if $(wildcard $@.cache),\
	@if [ $@ -ot $@.st -o ! -e $@ ]; then\
			echo "downloading $@";\
			cp $@.cache $@;\
			touch -d "$(shell cat $@.st)" $@;\
		else\
			echo "$@ is available locally";\
		fi;,\
	$(error $(subst .stfromcache,,$@) is promised but not in cache);)


# scenarios

a1 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 08:00:00 -0700" A.txt.cache
	@make -s A.txt.st
	@echo "make B.rds should pull A.txt, make B.rds/.st/.cache, not re-pull B.rds"
a2 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 08:00:00 -0700" A.txt.cache
	@make -s A.txt.st
	touch -d "2017-08-25 10:00:00 -0700" B.rds
	@echo "make B.rds should pull A.txt, make B.rds/.st/.cache, not re-pull B.rds"
a3 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 08:00:00 -0700" A.txt.cache
	@make -s A.txt.st
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	@make -s B.rds.stfromcache
	@echo "make B.rds should pull B.rds"
a4 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 08:00:00 -0700" A.txt.cache
	@make -s A.txt.st
	touch -d "2017-08-25 11:00:00 -0700" A.txt
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	@make -s B.rds.stfromcache
	@echo "make B.rds should pull B.rds from cache"
a5 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 08:00:00 -0700" A.txt.cache
	@make -s A.txt.st
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	@make -s B.rds.stfromcache
	touch -d "2017-08-25 07:00:00 -0700" B.rds
	@echo "make B.rds should pull B.rds from cache"
a6 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 08:00:00 -0700" A.txt.cache
	@make -s A.txt.st
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	@make -s B.rds.stfromcache
	touch -d "2017-08-25 10:00:00 -0700" B.rds
	@echo "make B.rds should do nothing"
a7 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 08:00:00 -0700" A.txt.cache
	@make -s A.txt.st
	touch -d "2017-08-25 08:00:00 -0700" A.txt
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	@make -s B.rds.stfromcache
	touch -d "2017-08-25 10:00:00 -0700" B.rds
	@echo "make B.rds should do nothing"
b1 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 11:00:00 -0700" A.txt.cache
	@make -s A.txt.st
	touch -d "2017-08-25 10:00:00 -0700" B.rds
	@echo "make B.rds should pull A.txt, make B.rds/.st/.cache, not re-pull B.rds"
b2 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 11:00:00 -0700" A.txt.cache
	@make -s A.txt.st
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	@make -s B.rds.stfromcache
	@echo "make B.rds should pull A.txt, make B.rds/.st/.cache, not re-pull B.rds"
b3 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 11:00:00 -0700" A.txt.cache
	@make -s A.txt.st
	touch -d "2017-08-25 11:00:00 -0700" A.txt
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	@make -s B.rds.stfromcache
	@echo "make B.rds should make B.rds/.st/.cache, not re-pull B.rds"
b4 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 11:00:00 -0700" A.txt.cache
	@make -s A.txt.st
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	@make -s B.rds.stfromcache
	touch -d "2017-08-25 12:00:00 -0700" B.rds
	@echo "make B.rds should pull A.txt, make B.rds/.st/.cache, not re-pull B.rds"
c1 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 10:00:00 -0700" B.rds
	@echo "make B.rds should give error about missing A.txt.cache"
c2 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	@make -s B.rds.stfromcache
	@echo "make B.rds should give error about missing A.txt.cache"
c3 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	@make -s B.rds.stfromcache
	touch -d "2017-08-25 11:00:00 -0700" B.rds
	@echo "make B.rds should give error about missing A.txt.cache"
c4 :
	rm -f A.txt* B.rds*
	touch -d "2017-08-25 11:00:00 -0700" A.txt.cache
	@make -s A.txt.st
	rm A.txt.cache
	touch -d "2017-08-25 09:00:00 -0700" B.rds.cache
	@make -s B.rds.stfromcache
	@echo "make B.rds should give error about missing A.txt.cache"

clean:
	rm -f A.txt* B.rds*
