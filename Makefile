# arch-tag: Main Makefile
# Copyright (C) 2004 - 2005 John Goerzen <jgoerzen@complete.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

.PHONY: all
all: setup
	./setup configure
	./setup build

PYTHON ?= python
setup: Setup.lhs MissingPy.cabal
	ghc -package Cabal Setup.lhs -o setup

MissingPy.cabal: gencabal.py
	$(PYTHON) gencabal.py

clean:
	-./setup clean
	-rm -rf html `find . -name "*.o"` `find . -name "*.hi"` \
		`find . -name "*~"` *.a setup dist testsrc/runtests \
		MissingPy.cabal local-pkg
	-cd doc && $(MAKE) clean

.PHONY: local-pkg
local-pkg: all
	echo "[" > local-pkg
	cat .installed-pkg-config >> local-pkg
	echo "]" >> local-pkg

testsrc/runtests: local-pkg $(shell find . -name "*.hs") \
			$(shell find . -name "*.hsc")
	ghc6 -O2 -o testsrc/runtests -Ldist/build -odir dist/build \
	   -package-conf local-pkg \
	   -hidir dist/build -idist/build -itestsrc \
		-package HUnit -package MissingPy --make testsrc/runtests.hs

# dist/build/libHSMissingPy-*
test-ghc6: testsrc/runtests
	testsrc/runtests 

test-hugs:
	runhugs -98 +o -P$(PWD)/libsrc:$(PWD)/testsrc: testsrc/runtests.hs

interact-hugs:
	hugs -98 +o -P$(PWD)/libsrc:

interact-ghci: all
	ghci -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts -ilibsrc

interact: interact-hugs

test: test-ghc6

