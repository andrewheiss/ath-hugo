---
title: "Automatically zip up subdirectories with Make"
date: 2020-01-10
year: "2020"
month: "2020/01"
description: Use a Makefile to automatically zip up all subdirectories in a given folder *while also* accounting for dependencies
images: 
- /blog/2020/01/10/makefile-subdirectory-zips/makefile-subdirectory-zips.png
tags: 
  - blogdown
  - r
  - rmarkdown
  - markdown
  - make
slug: makefile-subdirectory-zips
---


[See this notebook on GitHub](https://github.com/andrewheiss/makefile-subdirectory-zips). You can (and should) download the project from there if you want to follow along and try this out.

-----

[tl;dr: Skip to the completed example.](#tldr-final-makefile)

-----

I use [**blogdown**](https://bookdown.org/yihui/blogdown) to generate the websites for [all the courses I teach](https://www.andrewheiss.com/teaching/), and it’s delightful to not have to worry about databases and server configurations. I use a [`Makefile`](https://www.gnu.org/software/make/) to run the requisite commands with `make deploy`, which creates a magical incantation: R, [**blogdown**](https://bookdown.org/yihui/blogdown), and [Hugo](https://gohugo.io/) parse [R Markdown](https://rmarkdown.rstudio.com/) files and generate a complete HTML site, which then gets synced [to my server](https://evalsp20.classes.andrewheiss.com/), all with minimal input from me.

There are occasional points of friction, though. One that I’ve suffered through for the past few years is the creation and distribution of zipped files. I teach students R, and R projects rarely consist of a single file. To make it easier to distribute problem sets and projects to students, I zip these subfolders into single files like `problem_set_1.zip`, so they just have to download one thing. Creating `.zip` files on macOS is trivial—right click on a folder, choose “Compress {foldername}”, and you’re done.

But it’s tedious when you have lots of folders to zip, and even more tedious to remember to rezip folders where you’ve made changes. SO MANY TIMES I’ve fixed errors in R scripts or data but then have forgotten to rezip the project, and students end up downloading the uncorrected version of projects. Moreover, macOS includes hidden `.DS_Store` files when it zips up folders, and R and RStudio create their own invisible files and folders, like `.Rhistory` and `.Rproj.user/`. To avoid shipping these out to students, I typically go to the terminal, manually delete the unwanted invisible files, and *then* zip up the directory. But once again, I regularly forget to do this and end up including unwanted files.

I figured that since I’m already using a `Makefile` to generate and upload the course website, I’d try to use the magical power of Make to automatically zip up project folders as I build the site *and* update them only if there are any changes. And it turns out that it’s possible, though the end result looks really cryptic (as do all `Makefiles`, really).

What follows here is a step-by-step didactic explanation of how I built a set of `Makefile` recipes to automatically zip up all the directories within a given directory, excluding invisible files (i.e. any file or directory that begins with a `.`), and only zipping up directories that have been modified since the last time Make was run. [You can also skip to the end to see the finished `Makefile`.](#tldr-final-makefile)

## Folder structure

The example here assumes the project subdirectories live in a folder called `static/projects/`, since that mimics **blogdown**/Hugo (any files in the `static/` directory do not get processed by knitr when you build the site). Here’s the example folder structure with three problem set projects:

``` text
static
└── projects
    ├── problem_set_1
    │   ├── data
    │   │   └── stuff.csv
    │   ├── problem_set_1.Rproj
    │   └── work.Rmd
    ├── problem_set_2
    │   ├── data
    │   │   └── other_stuff.csv
    │   ├── problem_set_2.Rproj
    │   └── work.Rmd
    └── problem_set_3
        ├── code.R
        ├── data
        │   └── more_stuff.csv
        ├── problem_set_3.Rproj
        └── work.Rmd
```

In the end, what I want is something like this, with `.zip` files for each of the problem set folders (starred):

``` text
static
└── projects
    ├── problem_set_1
    │   ├── data
    │   │   └── stuff.csv
    │   ├── problem_set_1.Rproj
    │   └── work.Rmd
    ├── ⭐️ problem_set_1.zip
    ├── problem_set_2
    │   ├── data
    │   │   └── other_stuff.csv
    │   ├── problem_set_2.Rproj
    │   └── work.Rmd
    ├── ⭐️ problem_set_2.zip
    ├── problem_set_3
    │   ├── code.R
    │   ├── data
    │   │   └── more_stuff.csv
    │   ├── problem_set_3.Rproj
    │   └── work.Rmd
    └── ⭐️ problem_set_3.zip
```

## Basic approach

The basic syntax for `Makefile` recipes is fairly simple:

``` make
file_to_create: dependencies
    stuff to run to create file using dependencies
```

If I want to create a zipped file of one problem set project, I can add this to a file named `Makefile` (no extension), and then run `make static/projects/problem_set_1.zip` from the terminal:

``` make
static/projects/problem_set_1.zip:
    zip -r static/projects/problem_set_1.zip static/projects/problem_set_1
```

The `-r` flag means that all subdirectories in `static/projects/problem_set_1` will be included. If I type `make static/projects/problem_set_1.zip` from the terminal, it should compress the folder and all its subfolders into a single `.zip` file.

``` text
> make static/projects/problem_set_1.zip

zip -r static/projects/problem_set_1.zip static/projects/problem_set_1
  adding: static/projects/problem_set_1/ (stored 0%)
  adding: static/projects/problem_set_1/work.Rmd (stored 0%)
  adding: static/projects/problem_set_1/.DS_Store (deflated 97%)
  adding: static/projects/problem_set_1/problem_set_1.Rproj (deflated 28%)
  adding: static/projects/problem_set_1/data/ (stored 0%)
  adding: static/projects/problem_set_1/data/stuff.csv (stored 0%)
```

## Problems with the basic approach

However, there are a few issues:

1.  If I run this again, `zip` will add files to the `.zip`. If any were deleted from the actual folder, they’ll stay inside the `.zip` file (i.e. the folder and the `.zip` file won’t be synchronized).
2.  This will include files and folders that begin with `.` (like `.Rhistory` and `.Rproj.user/`), and I don’t want those.
3.  The command will include all the parent folders in the zipped file. After I extract it, the actual files will be nested inside `static/projects/`.
4.  There are no dependencies, so this will happen every time I run `make static/projects/problem_set_1.zip`, even if nothing changed.
5.  This is for a single target only. In theory, I’d need to manually add separate entries for each folder in `static/projects`, and that’s tedious.

## Tweaking the `zip` recipe

We can fix the first two issues with some adjustments to the `zip` command. Adding the `-FS` flag (*f*ile *s*ync) will ensure that `zip` syncs the files between the source directory and the zipped files, and including the `-x` flag allows us to exclude patterns of files from the zipped file. Doing this will both sync files and exclude files/folders with a `.` prefix:

``` make
static/projects/problem_set_1.zip:
    zip -FSr static/projects/problem_set_1.zip static/projects/problem_set_1 -x static/projects/problem_set_1/.\*
```

``` text
> make static/projects/problem_set_1.zip

zip -FSr static/projects/problem_set_1.zip static/projects/problem_set_1 -x static/projects/problem_set_1/.\*
  adding: static/projects/problem_set_1/ (stored 0%)
  adding: static/projects/problem_set_1/work.Rmd (stored 0%)
  adding: static/projects/problem_set_1/problem_set_1.Rproj (deflated 28%)
  adding: static/projects/problem_set_1/data/ (stored 0%)
  adding: static/projects/problem_set_1/data/stuff.csv (stored 0%)
```

The third issue with parent folders can’t be solved directly with `zip`. The problem occurs because we’re running `make` from the root of the project, so it’s including the whole nested file structure. To solve this, we can have `make` navigate to `static/projects` before zipping anything, which then simplifies the filenames in the command:

``` make
static/projects/problem_set_1.zip:
    cd static/projects && zip -FSr problem_set_1.zip problem_set_1 -x problem_set_1/.\*
```

``` text
> make static/projects/problem_set_1.zip

cd static/projects && zip -FSr problem_set_1.zip problem_set_1 -x problem_set_1/.\*
  adding: problem_set_1/ (stored 0%)
  adding: problem_set_1/work.Rmd (stored 0%)
  adding: problem_set_1/problem_set_1.Rproj (deflated 28%)
  adding: problem_set_1/data/ (stored 0%)
  adding: problem_set_1/data/stuff.csv (stored 0%)
```

No more nested parent folders\!

## Adding dependencies

Right now, there are no dependencies, which means that `make` will run `zip` regardless of whether it needs to. Even if nothing is changed in the `problem_set_1` folder, a new `.zip` file will get created (and it’ll get reuploaded to the server because it’ll have a new creation date, which means there will be a lot of unnecessary uploading of potentially large files).

Normally, we’d have to define dependencies like this:

``` make
static/projects/problem_set_1.zip: {DEPENDENT FILES}
    cd static/projects && zip -FSr problem_set_1.zip problem_set_1 -x problem_set_1/.\*
```

There’s no way to use a folder as a dependency, which means we need to list all the possible files in the project:

``` make
static/projects/problem_set_1.zip: static/projects/problem_set_1/work.Rmd static/projects/problem_set_1/problem_set_1.Rproj static/projects/problem_set_1/data/stuff.csv
    cd static/projects && zip -FSr problem_set_1.zip problem_set_1 -x problem_set_1/.\*
```

And that’s horrifically long and awful.

Instead of manually typing out all the dependencies, we can use a little `bash` trick to get a list of all the files in the folder and generate the list of dependencies using [`$(shell ...)`](https://www.gnu.org/software/make/manual/html_node/Shell-Function.html). To see this in action more easily, it’s helpful to assign this shell command to a variable and create a new temporary target to show it

``` make
FILES_IN_FOLDER = $(shell find static/projects/problem_set_1 -type f)

debug:
    @echo $(FILES_IN_FOLDER)
```

Now if you run `make debug`, you should see a list of all the files in the given folder (since that list was stored as `FILES_IN_FOLDER`; the `@` in front of `echo` suppresses the actual call to `echo`, so you should just see the output of the command).

``` text
> make debug

static/projects/problem_set_1/work.Rmd static/projects/problem_set_1/.DS_Store static/projects/problem_set_1/problem_set_1.Rproj static/projects/problem_set_1/data/stuff.csv
```

You might see that it found invisible files like `static/projects/problem_set_1/.DS_Store`. We don’t want to include those as dependencies (especially files in `.Rproj.user`, since those get modified every time you do anything in RStudio), so we can modify the `find` command to exclude files that start with `.`:

``` make
FILES_IN_FOLDER = $(shell find static/projects/problem_set_1 -type f ! -path "static/projects/problem_set_1/.*")

debug:
    @echo $(FILES_IN_FOLDER)
```

``` text
> make debug

static/projects/problem_set_1/work.Rmd static/projects/problem_set_1/problem_set_1.Rproj static/projects/problem_set_1/data/stuff.csv
```

With that `$(shell ...)` magic working, we can include it in the recipe as a dependency:

``` make
static/projects/problem_set_1.zip: $(shell find static/projects/problem_set_1 -type f ! -path "static/projects/problem_set_1/.*")
    cd static/projects && zip -FSr problem_set_1.zip problem_set_1 -x problem_set_1/.\*
```

HOWEVER, this still won’t quite work. Put simply (and likely incorrectly, but it makes sense) `make` expands `$()` variables after it determines dependency rules, so it will treat `$(shell ...)` like an actual dependency instead of treating the *output* of `$(shell ...)` as dependencies. To get around this, we can enable [secondary expansion](http://www.gnu.org/software/make/manual/make.html#Secondary-Expansion) ([see also](https://stackoverflow.com/a/21950971/120898)), which delays the creation of dependency rules until after `$(shell ...)` is run. To enable it, include `.SECONDEXPANSION:` somewhere and then use `$$()` instead of `$()` when expanding variables:

``` make
.SECONDEXPANSION:

static/projects/problem_set_1.zip: $$(shell find static/projects/problem_set_1 -type f ! -path "static/projects/problem_set_1/.*")
    cd static/projects && zip -FSr problem_set_1.zip problem_set_1 -x problem_set_1/.\*
```

``` text
> make static/projects/problem_set_1.zip

cd static/projects && zip -FSr problem_set_1.zip problem_set_1 -x problem_set_1/.\*
  adding: problem_set_1/ (stored 0%)
  adding: problem_set_1/work.Rmd (stored 0%)
  adding: problem_set_1/problem_set_1.Rproj (deflated 28%)
  adding: problem_set_1/data/ (stored 0%)
  adding: problem_set_1/data/stuff.csv (stored 0%)
```

Phew. Now all the non-invisible files inside `problem_set_1/` are dependencies. If I run it again without changing anything, nothing should get zipped:

``` text
> make static/projects/problem_set_1.zip

make: `static/projects/problem_set_1.zip' is up to date.
```

## Automatic variables

Everything’s working great so far\! We’ve addressed [the first 4 of the 5 issues we found before](#problems-with-the-basic-approach). The only thing we have left is automating this so we don’t have to make another target recipe for `static/projects/problem_set_2.zip` and `static/projects/problem_set_3.zip` and so on. In the end, we want to be able to type `make zip_projects` and have `make` compress each of the subfolders automatically, based on changes in dependencies. To do this, we can use [automatic variables](http://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html) ([see also](https://stackoverflow.com/a/3220288/120898)) to generate targets on the fly. Here’s how we do this.

First, we can generate a list of all the subdirectories we want to compress, and then modify that list so that it becomes a list of all the targets we want to create (i.e. `problem_set_1.zip`, `problem_set_2.zip`, and so on). We’ll use some built-in `make` functions for manipulating text and searching for files to create some variables. Again, it’s easiest to see what these variables actually are if you create a temporary target like debug. Check the documentation for [`$(filter ...)`](https://www.gnu.org/software/make/manual/html_node/Text-Functions.html), [`$(wildcard ...)`](https://www.gnu.org/software/make/manual/html_node/File-Name-Functions.html), [`$(patsubst ...)`](https://www.gnu.org/software/make/manual/html_node/Text-Functions.html), and [`$(addsuffix ...)`](https://www.gnu.org/software/make/manual/html_node/File-Name-Functions.html) for more details about what these functions do.

``` make
TO_ZIP_DIRS = $(filter %/, $(wildcard static/projects/*/))  # Find all directories in static/projects
TO_ZIP_NAMES = $(patsubst %/,%,$(TO_ZIP_DIRS))  # Remove trailing /
ZIP_TARGETS = $(addsuffix .zip,$(TO_ZIP_NAMES))  # Add .zip

debug:
    @echo $(TO_ZIP_DIRS)
    @echo $(TO_ZIP_NAMES)
    @echo $(ZIP_TARGETS)
```

``` text
> make debug

static/projects/problem_set_1/ static/projects/problem_set_2/ static/projects/problem_set_3/
static/projects/problem_set_1 static/projects/problem_set_2 static/projects/problem_set_3
static/projects/problem_set_1.zip static/projects/problem_set_2.zip static/projects/problem_set_3.zip
```

Neat. `$(ZIP_TARGETS)` now has a list of zipped files that we want to create. We can use that list as an actual target in a recipe. For now, we’ll just use `echo` so we can see what’s going on with the variable names. Note how I created a new target called `zip_projects`—this is what we’ll actually run in the terminal. It will look at the list in `$(ZIP_TARGETS)` and run the appropriate recipe for each one (which for now means it’ll just print the name of the file). Also notice the `$@`. This represents the name of the target that is passed to the recipe. Run `make zip_projects` and you should see a list of future filenames:

``` make
$(ZIP_TARGETS):
    @echo $@

zip_projects: $(ZIP_TARGETS)
```

``` text
> make zip_projects

static/projects/problem_set_1.zip
static/projects/problem_set_2.zip
static/projects/problem_set_3.zip
```

Next we need to modify the big hairy `cd static/projects && zip -FSr problem_set_1.zip problem_set_1 -x problem_set_1/.\*` command to use the automatic names in `$(ZIP_TARGETS)`. We have access to the full target name (`static/projects/problem_set_1.zip`) as the special `$@` variable. We can use other built-in functions to extract pieces of that string. Specifically, we need these things:

  - `static/projects`, or the level below the base name of the future `.zip` file
  - `problem_set_1.zip`, or the parent-directory-less name of the future `.zip` file
  - `problem_set_1`, or the parent-directory-less and extension-less name of the future `.zip` file

We can extract each of those with different [file name functions](https://www.gnu.org/software/make/manual/html_node/File-Name-Functions.html):

  - `$(basename $@)` will lead to `static/projects/problem_set_1`. We can navigate to this folder with `cd` and then move back a level with `..` to get `static/projects`
  - `$(notdir $@)` will lead to `problem_set_1.zip`
  - `$(notdir $(basename $@))` will lead to `problem_set_1`

You can check all these by adding them to the recipe temporarily:

``` make
$(ZIP_TARGETS):
    @echo $@
    @echo $(basename $@)
    @echo $(notdir $@)
    @echo $(notdir $(basename $@))

zip_projects: $(ZIP_TARGETS)
```

``` text
> make zip_projects

static/projects/problem_set_1.zip
static/projects/problem_set_1
problem_set_1.zip
problem_set_1
static/projects/problem_set_2.zip
static/projects/problem_set_2
problem_set_2.zip
problem_set_2
static/projects/problem_set_3.zip
static/projects/problem_set_3
problem_set_3.zip
problem_set_3
```

With all those pieces of filenames, we can replace the hardcoded values of our hairy `zip` command with automatic versions:

``` make
$(ZIP_TARGETS):
    cd $(basename $@)/.. && zip -FSr $(notdir $@) $(notdir $(basename $*)) -x $(notdir $(basename $*))/.\*
```

``` text
> make zip_projects

cd static/projects/problem_set_1/.. && zip -FSr problem_set_1.zip problem_set_1 -x problem_set_1/.\*
  adding: problem_set_1/ (stored 0%)
  adding: problem_set_1/work.Rmd (stored 0%)
  adding: problem_set_1/problem_set_1.Rproj (deflated 28%)
  adding: problem_set_1/data/ (stored 0%)
  adding: problem_set_1/data/stuff.csv (stored 0%)
cd static/projects/problem_set_2/.. && zip -FSr problem_set_2.zip problem_set_2 -x problem_set_2/.\*
  adding: problem_set_2/ (stored 0%)
  adding: problem_set_2/work.Rmd (stored 0%)
  adding: problem_set_2/problem_set_2.Rproj (deflated 28%)
  adding: problem_set_2/data/ (stored 0%)
  adding: problem_set_2/data/other_stuff.csv (stored 0%)
cd static/projects/problem_set_3/.. && zip -FSr problem_set_3.zip problem_set_3 -x problem_set_3/.\*
  adding: problem_set_3/ (stored 0%)
  adding: problem_set_3/work.Rmd (stored 0%)
  adding: problem_set_3/code.R (stored 0%)
  adding: problem_set_3/problem_set_3.Rproj (deflated 28%)
  adding: problem_set_3/data/ (stored 0%)
  adding: problem_set_3/data/more_stuff.csv (stored 0%)
```

Holy moly\! It zipped each individual folder\! We’re almost done\! The only thing we’re missing is dependencies—right now, if we run this again, it’ll rezip everything again, which we don’t want. We need to add our `$$(shell ..)` incantation as a dependency, but we need to make it specific to each target: i.e. it needs to generate a list of dependent files for each of the future zip files (only the contents of `problem_set_1` when making `problem_set_1.zip`, etc.). To make that work, we can use a `%` wildcard in the dependency definition:

``` make
$(ZIP_TARGETS): %.zip : $$(shell find % -type f ! -path "%/.*")
    cd $(basename $@)/.. && zip -FSr $(notdir $@) $(notdir $(basename $@)) -x $(notdir $(basename $@))/.\*
```

If we manually delete any zipped files and run `make zip_projects`, it’ll generate them as expected. Where this is magical, though, is if I edit one of the files (like `word.Rmd` in `problem_set_1`) and then rerun `make zip_projects`, it will *only* rezip that project:

``` text
> make zip_projects

cd static/projects/problem_set_1/.. && zip -FSr problem_set_1.zip problem_set_1 -x problem_set_1/.\*
updating: problem_set_1/work.Rmd (stored 0%)
```

And that’s it\! We’re done\!

The final recipe is *extraordinarily* cryptic, but because we built it up slowly, we should know what each of the pieces (`$@`, `%`, `$(ZIP_TARGETS)`, `$(basename $@)`, etc.) are.

### tl;dr final `Makefile`

Here’s the complete final `Makefile` without all the intermediate steps:

``` make
TO_ZIP_DIRS = $(filter %/, $(wildcard static/projects/*/))  # Find all directories in static/projects
TO_ZIP_NAMES = $(patsubst %/,%,$(TO_ZIP_DIRS))  # Remove trailing /
ZIP_TARGETS = $(addsuffix .zip,$(TO_ZIP_NAMES))  # Add .zip

.SECONDEXPANSION:

$(ZIP_TARGETS): %.zip : $$(shell find % -type f ! -path "%/.*")
    cd $(basename $@)/.. && zip -FSr $(notdir $@) $(notdir $(basename $@)) -x $(notdir $(basename $@))/.\*

zip_projects: $(ZIP_TARGETS)
```

You can incorporate this into any other `Makefile`, [like this one that I use for building a course website](https://github.com/andrewheiss/evalsp20.classes.andrewheiss.com/blob/master/Makefile). There, the `zip_projects` target is a dependency of the `build` target, so I just have to run `make build` to automatically zip everything up *and* build the site with **blogdown**.

Perfection. Any subfolder in `static/projects/` will automatically be zipped up with no input from me. No more accidentally forgetting to zip up things or include invisible files\!
