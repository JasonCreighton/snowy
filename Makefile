.PHONY: debug release safe clean cppcheck clang-tidy analyze lint all

debug:
	+make -f Makefile.debug
release:
	+make -f Makefile.release
safe:
	+make -f Makefile.safe
clean:
	+make -f Makefile.debug clean
	+make -f Makefile.release clean
	+make -f Makefile.safe clean

compile_commands.json: Makefile.debug
	make -f Makefile.debug clean
	bear make -f Makefile.debug

cppcheck:
	cppcheck --quiet --enable=all --error-exitcode=1 .

clang-tidy: compile_commands.json
	run-clang-tidy-3.9.py -j 4 -checks='*,-google-*,-llvm-*,-cppcoreguidelines-*,-clang-analyzer-*,-cert-*,-readability-else-after-return,-readability-implicit-bool-cast'

analyze: Makefile.debug
	make -f Makefile.debug clean
	scan-build-3.9 make -f Makefile.debug -j 4

lint: clang-tidy cppcheck

all: debug release safe
