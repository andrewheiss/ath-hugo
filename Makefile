OUTPUTDIR=public
SSH_TARGET=cloud:/home/andrew/sites/andrewheiss.com/public_html/

## clean	:	Delete public/
clean:
	rm -rf public/

## build	:	Build site with blogdown through R
build: 
	Rscript -e "blogdown::build_site(build_rmd = blogdown::filter_md5sum)"

## serve	:	Serve site at 127.0.0.1:4321
serve: 
	hugo server --bind 127.0.0.1 -p 4321 --renderToDisk

## blog	:	Create new blog post in blog/ (make blog ARGS="folder_name")
blog:
	hugo new --kind blog-bundle blog/$(ARGS)

## research:	Create new research page in research/ (make research ARGS="articles/project_name")
research:
	hugo new --kind research-bundle research/$(ARGS)

## deploy	:	Build and upload site to andrewheiss.com with rsync
deploy: build
	rsync -Prvzc --exclude='.DS_Store' --exclude='.Rproj.user/' --delete $(OUTPUTDIR)/ $(SSH_TARGET)

# Self-documenting Makefiles from The Carpentries
# https://swcarpentry.github.io/make-novice/08-self-doc/index.html
## help	:	Show possible targets
.PHONY: help
help: Makefile
	@sed -n 's/^##//p' $<
