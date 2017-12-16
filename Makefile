# Path to scan-build-py's analyze-build, which uses the compilation database
# to drive analysis (rather than intercepting build commands)
CLANG_ANALYZE_BUILD=/usr/share/clang/scan-build-py-3.9/bin/analyze-build

.PHONY: debug release safe win64 clean cppcheck clang-tidy analyze lint all

debug:
	+make -f Makefile.debug
release:
	+make -f Makefile.release
safe:
	+make -f Makefile.safe
win64:
	+make -f Makefile.win64
clean:
	+make -f Makefile.debug clean
	+make -f Makefile.release clean
	+make -f Makefile.safe clean
	+make -f Makefile.win64 clean
	rm -f compile_commands.json

compile_commands.json: Makefile.debug
	make -f Makefile.debug clean
	bear make -f Makefile.debug

cppcheck:
	cppcheck --quiet --enable=all --error-exitcode=1 .

clang-tidy: compile_commands.json
	run-clang-tidy-3.9.py -j 4 -checks='*,-google-*,-llvm-*,-cppcoreguidelines-*,-clang-analyzer-*,-cert-*,-readability-else-after-return,-readability-implicit-bool-cast'

clang-analyze: compile_commands.json
	$(CLANG_ANALYZE_BUILD) --status-bugs

lint: cppcheck clang-analyze clang-tidy

all: debug release safe
