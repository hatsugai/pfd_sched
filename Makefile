all:
	find . -name '*.coverage' | xargs rm -f
	dune runtest --instrument-with bisect_ppx --force

report:
	bisect-ppx-report html

summary:
	bisect-ppx-report summary

clean:
	find . -name '*.coverage' | xargs rm -f
