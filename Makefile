
Readme.md: Data/Aeson/Util.lhs Makefile
	pandoc -f markdown+lhs -t markdown < $< \
		| sed 's/~~~~/```/' \
		| sed 's/\ {\.sourceCode\ \.literate\ \.haskell}/haskell/g' \
		> $@

clean:
	rm -f Readme.md
