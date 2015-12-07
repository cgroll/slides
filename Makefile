TMPL = pandoc_custom/templates/revealjs.template
CSL = pandoc_custom/csl/elsevier-harvard.csl

# output files, file stem
FILES = assetMgmt.slides.html assetMgmt.pdf
OUTDIR = output

# speficy default target
CURRENT_TARGET = $(OUTDIR)/assetMgmt
current: $(CURRENT_TARGET).slides.html

reveal: $(CURRENT_TARGET).slides.html
pdf: $(CURRENT_TARGET).pdf

# target to build all formats for current target
all: $(OUTDIR)/assetMgmt.slides.html  $(OUTDIR)/assetMgmt.pdf $(OUTDIR)/pkgAssetMgmt.slides.html

# target to build all files
OUT := $(addprefix $(OUTDIR)/,$(FILES))
everything: $(OUT)

debug: $(CURRENT_TARGET).tex

###############
## assetMgmt ##
###############

$(OUTDIR)/assetMgmt.slides.html: slide_srcs/assetMgmt.md Makefile refs.bib
	pandoc --template=$(TMPL) \
	--slide-level=3 --toc --toc-depth=1 \
	--filter pandoc_custom/filters/adaptHeaders.hs \
	--filter pandoc_custom/filters/amsmath.hs \
	--variable theme="black" \
	-V slideNumber=true \
	--include-in-header=pandoc_custom/css/reveal_left_strong.css \
	-s -V revealjs-url=../reveal.js -t revealjs -f markdown \
	--filter pandoc-citeproc --csl=$(CSL) \
	--bibliography=refs.bib \
	-o $@ $<

#	-V transition=none \

$(OUTDIR)/assetMgmt.pdf: slide_srcs/assetMgmt.md Makefile refs.bib
	pandoc -s -t beamer -f markdown \
	--slide-level=3 \
	-V theme=CambridgeUS -V colortheme=dolphin \
	--mathjax \
	--filter pandoc_custom/filters/skip_pause.hs \
	--filter pandoc-citeproc --csl=pandoc_custom/csl/elsevier-harvard.csl \
	--bibliography=refs.bib \
	-o $@ $<

# -V theme=Frankfurt -V colortheme=default \

$(OUTDIR)/assetMgmt.tex: slide_srcs/assetMgmt.md Makefile refs.bib
	pandoc -s -t beamer -f markdown \
	-V theme=CambridgeUS -V colortheme=dolphin \
	--mathjax \
	--filter pandoc_custom/filters/skip_pause.hs \
	--filter pandoc-citeproc --csl=pandoc_custom/csl/elsevier-harvard.csl \
	--bibliography=refs.bib \
	-o $@ $<

#####################
## Julia AssetMgmt ##
#####################

$(OUTDIR)/pkgAssetMgmt.slides.html: slide_srcs/pkgAssetMgmt.md Makefile
	pandoc --template=$(TMPL) \
	--slide-level=3 --toc --toc-depth=1 \
	--filter pandoc_custom/filters/adaptHeaders.hs \
	--filter pandoc_custom/filters/amsmath.hs \
	--variable theme="black" \
	-V slideNumber=true \
	--include-in-header=pandoc_custom/css/reveal_left_strong.css \
	-s -V revealjs-url=../reveal.js -t revealjs -f markdown \
	-o $@ $<

################
## publishing ##
################

publish: $(OUT) Makefile refs.bib
	git checkout gh-pages
	git checkout master output/assetMgmt.slides.html
	git commit -m "asset management slides updated"
	git push origin gh-pages
	git checkout master

# TODO: push files to ftp server

clean:
	rm -f $(OUTDIR)/*.slides.html \
	rm -f $(OUTDIR)/*.pdf

again:
	make clean
	make
