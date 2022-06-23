---
title: "Quick and easy ways to deal with long labels in ggplot2"
date: 2022-06-23
year: "2022"
month: "2022/06"
description: "Explore different manual and automatic ways to rotate, dodge, recode, break up, and otherwise deal with long axis labels with ggplot2"
images: 
- /blog/2022/06/23/long-labels-ggplot/index_files/figure-html/plot-all-1.png
tags:
  - r
  - tidyverse
  - ggplot
  - data visualization
slug: long-labels-ggplot
editor_options:
  chunk_output_type: console
---



In [one of the assignments](https://datavizs22.classes.andrewheiss.com/assignment/04-exercise/) for my [data visualization class](https://datavizs22.classes.andrewheiss.com/), I have students visualize the number of [essential construction projects](https://www1.nyc.gov/assets/buildings/html/essential-active-construction.html) that were allowed to continue during New York City's initial COVID shelter-in-place order in March and April 2020. It's a good dataset to practice visualizing amounts and proportions and to practice with **dplyr**'s `group_by()` and `summarize()` and shows some interesting trends.

The data includes a column for `CATEGORY`, showing the type of construction project that was allowed. It poses an interesting (and common!) visualization challenge: some of the category names are really long, and if you plot `CATEGORY` on the x-axis, the labels overlap and become unreadable, like this: 


```r
library(tidyverse)  # dplyr, ggplot2, and friends
library(scales)     # Functions to format things nicely

# Load pandemic construction data
essential_raw <- read_csv("https://datavizs22.classes.andrewheiss.com/projects/04-exercise/data/EssentialConstruction.csv")

essential_by_category <- essential_raw %>%
  # Calculate the total number of projects within each category
  group_by(CATEGORY) %>%
  summarize(total = n()) %>%
  # Sort by total
  arrange(desc(total)) %>%
  # Make the category column ordered
  mutate(CATEGORY = fct_inorder(CATEGORY))
```


```r
ggplot(essential_by_category,
       aes(x = CATEGORY, y = total)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Total projects")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/plot-the-problem-1.png" width="75%" style="display: block; margin: auto;" />

Ew. The middle categories here get all blended together into an unreadable mess.

Fortunately there are a bunch of different ways to fix this, each with their own advantages and disadvantages!


## Contents <!-- omit in toc -->

- [Option A: Make the plot wider](#option-a-make-the-plot-wider)
- [Option B: Swap the x- and y-axes](#option-b-swap-the-x--and-y-axes)
- [Option C: Recode some longer labels](#option-c-recode-some-longer-labels)
- [Option D: Rotate the labels](#option-d-rotate-the-labels)
- [Option E: Dodge the labels](#option-e-dodge-the-labels)
- [Option F: Automatically add line breaks](#option-f-automatically-add-line-breaks)
- [Summary](#summary)


## Option A: Make the plot wider

One quick and easy way to fix this is to change the dimensions of the plot so that there's more space along the x-axis. If you're using R Markdown or Quarto, you can modify the chunk options and specify `fig.width`:


````default
```{r name-of-chunk, fig.width=10, fig.height=4}
ggplot(essential_by_category,
       aes(x = CATEGORY, y = total)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Total projects")
```
````

<img src="{{< blogdown/postref >}}index_files/figure-html/plot-wider-1.png" width="75%" style="display: block; margin: auto;" />

If you're using `ggsave()`, you can specify the height and width there too:


```r
ggsave(name_of_plot, width = 10, height = 4, units = "in")
```

That works, but now the font is tiny, so we need to adjust it up with `theme_gray(base_size = 18)`:


````default
```{r name-of-chunk, fig.width=10, fig.height=4}
ggplot(essential_by_category,
       aes(x = CATEGORY, y = total)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Total projects") +
  theme_gray(base_size = 18)
```
````

<img src="{{< blogdown/postref >}}index_files/figure-html/plot-wider-bigger-1.png" width="75%" style="display: block; margin: auto;" />

Now the font is bigger, but the labels overlap again! We could make the figure wider again, but then we'd need to increase the font size again, and now we're in an endless loop.

**Verdict: 2/10, easy to do, but more of a quick band-aid-style solution; not super recommended.**


## Option B: Swap the x- and y-axes

Another quick and easy solution is to switch the x- and y-axes. If we put the categories on the y-axis, each label will be on its own line so the labels can't overlap with each other anymore:


```r
ggplot(essential_by_category,
       aes(y = fct_rev(CATEGORY), x = total)) +
  geom_col() +
  scale_x_continuous(labels = comma) +
  labs(y = NULL, x = "Total projects")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/plot-swap-1.png" width="75%" style="display: block; margin: auto;" />

That works really well! However, it forces you to work with horizontal bars. If that doesn't fit with your overall design (e.g., if you really want vertical bars), this won't work. Additionally, if you have any really long labels, it can substantially shrink the plot area, like this:


```r
# Make one of the labels super long for fun
essential_by_category %>%
  mutate(CATEGORY = recode(CATEGORY, "Schools" = "Preschools, elementary schools, middle schools, high schools, and other schools")) %>%
  ggplot(aes(y = fct_rev(CATEGORY), x = total)) +
  geom_col() +
  scale_x_continuous(labels = comma) +
  labs(y = NULL, x = "Total projects")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/plot-swap-long-1.png" width="75%" style="display: block; margin: auto;" />

**Verdict: 6/10, easy to do and works well if you're happy with horizontal bars; can break if labels are too long (though long y-axis labels are fixable with the other techniques in this post too).**


## Option C: Recode some longer labels

Instead of messing with the width of the plot, we can mess with the category names themselves. We can use `recode()` from **dplyr** to recode some of the longer category names or add line breaks (`\n`) to them:


```r
essential_by_category_shorter <- essential_by_category %>%
  mutate(CATEGORY = recode(CATEGORY, 
                           "Affordable Housing" = "Aff. Hous.",
                           "Hospital / Health Care" = "Hosp./Health",
                           "Public Housing" = "Pub. Hous.",
                           "Homeless Shelter" = "Homeless\nShelter"))

ggplot(essential_by_category_shorter,
       aes(x = CATEGORY, y = total)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Total projects")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/plot-manual-1.png" width="75%" style="display: block; margin: auto;" />

That works great! However, it reduces readibility (does "Aff. Hous." mean affordable housing? affluent housing? affable housing?). It also requires more manual work and a lot of extra typing. If a new longer category gets added in a later iteration of the data, this code won't automatically shorten it.

**Verdict: 6/10, we have more control over the labels, but too much abbreviation reduces readibility, and it's not automatic.**


## Option D: Rotate the labels

Since we want to avoid manually recoding categories, we can do some visual tricks to make the labels readable without changing any of the lable text. First we can rotate the labels a little. Here we rotate the labels 30°, but we could also do 45°, 90°, or whatever we want. If we add `hjust = 0.5` (horizontal justification), the rotated labels will be centered in the columns, and `vjust` (vertical justification) will center the labels vertically.


```r
ggplot(essential_by_category,
       aes(x = CATEGORY, y = total)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Total projects") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/plot-rotate-1.png" width="75%" style="display: block; margin: auto;" />

Everything fits great now, but I'm not a big fan of angled text. I'm also not happy with the all the empty vertical space between the axis and the shorter labels like "Schools" and "Utility". It would look a lot nicer to have all these labels right-aligned to the axis, but there's no way easy to do that.

**Verdict: 5.5/10, no manual work needed, but angled text is harder to read and there's lots of extra uneven whitespace.**


## Option E: Dodge the labels

Second, instead of rotating, as of [**ggplot2** v3.3.0](https://www.tidyverse.org/blog/2020/03/ggplot2-3-3-0/) we can automatically dodge the labels and make them offset across multiple rows with the `guide_axis(n.dodge = N)` function in `scale_x_*()`:


```r
ggplot(essential_by_category,
       aes(x = CATEGORY, y = total)) +
  geom_col() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Total projects")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/plot-dodge-1.png" width="75%" style="display: block; margin: auto;" />

That's pretty neat. Again, this is all automatic and we don't have to manually adjust any labels. The text is all horizontal so it's more readable. But I'm not a huge fan of the gaps above the second-row labels. Maybe it would look better if the corresponding axis ticks were a little longer, idk.

**Verdict: 7/10, no manual work needed, labels easy to read, but there's extra whitespace that can sometimes feel unbalanced.**


## Option F: Automatically add line breaks

The easiest and quickest and nicest way to fix these long labels, though, is to use the `label_wrap()` function from the **scales** package. This will automatically add line breaks after X characters in labels with lots of text—you just have to tell it how many characters to use. The function is smart enough to try to break after word boundaries—that is, if you tell it to break after 5 characters, it won't split something like "Approved" into "Appro" and "ved"; it'll break after the end of the word.


```r
ggplot(essential_by_category,
       aes(x = CATEGORY, y = total)) +
  geom_col() +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Total projects")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/plot-auto-1.png" width="75%" style="display: block; margin: auto;" />

Look at how the x-axis labels automatically break across lines! That's so neat!

**Verdict: 11/10, no manual work needed, labels easy to read, everything's perfect. [This is the way.](https://www.starwars.com/news/this-is-the-way-the-mandalorian-art)**

Bonus: For things that aren't axis labels, like titles and subtitles, you can use `str_wrap()` from **stringr** to break long text at X characters (specified with `width`):


```r
ggplot(essential_by_category,
       aes(x = CATEGORY, y = total)) +
  geom_col() +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Total projects",
       title = str_wrap(
         "Here's a really long title that will go off the edge of the figure unless it gets broken somewhere", 
         width = 50),
       subtitle = str_wrap(
         "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
         width = 70))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/plot-auto-str-wrap-1.png" width="75%" style="display: block; margin: auto;" />


## Summary

Here's a quick comparison of all these different approaches:

<img src="{{< blogdown/postref >}}index_files/figure-html/plot-all-1.png" width="100%" style="display: block; margin: auto;" />
