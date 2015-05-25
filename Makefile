all:
	@rm -rf cdn
	@rm -rf out
	@lein with-profile cdn cljsbuild once
	@gulp dist
