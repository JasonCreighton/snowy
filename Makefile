# Path to scan-build-py's analyze-build, which uses the compilation database
# to drive analysis (rather than intercepting build commands)
CLANG_ANALYZE_BUILD=/usr/share/clang/scan-build-py-3.9/bin/analyze-build

.PHONY: debug release safe win64 clean cppcheck clang-tidy analyze lint all

release:
	+make -f Makefile.common CONFIG=release

debug:
	+make -f Makefile.common CONFIG=debug

profile:
	+make -f Makefile.common CONFIG=profile

safe:
	+make -f Makefile.common CONFIG=safe

win64:
	+make -f Makefile.common CONFIG=win64

clean:
	+make -f Makefile.common CONFIG=debug clean
	+make -f Makefile.common CONFIG=profile clean
	+make -f Makefile.common CONFIG=release clean
	+make -f Makefile.common CONFIG=safe clean
	+make -f Makefile.common CONFIG=win64 clean
	rm -f compile_commands.json

compile_commands.json: Makefile Makefile.common
	make -f Makefile.common CONFIG=debug clean
	bear make -f Makefile.common CONFIG=debug

cppcheck:
	cppcheck --quiet --enable=all --error-exitcode=1 .

clang-tidy: compile_commands.json
	run-clang-tidy-3.9.py -j 4 -checks='*,-google-*,-llvm-*,-cppcoreguidelines-*,-clang-analyzer-*,-cert-*,-readability-else-after-return,-readability-implicit-bool-cast,-readability-simplify-boolean-expr,-modernize-raw-string-literal'

clang-analyze: compile_commands.json
	$(CLANG_ANALYZE_BUILD) --status-bugs

lint: cppcheck clang-analyze clang-tidy

all: debug release safe
