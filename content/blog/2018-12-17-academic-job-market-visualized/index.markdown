---
title: The academic job search finally comes to an end
date: 2018-12-17
year: "2018"
month: "2018/12"
description: Explore 2.5 years of applying for academic jobs with fancy data visualization
tags: 
  - r
  - ggplot
  - dataviz
  - jobs
slug: academic-job-market-visualized
---



I am *so beyond thrilled* to announce that I'll be joining the [Andrew Young School of Policy Studies](https://aysps.gsu.edu/) at [Georgia State University](https://www.gsu.edu/) in Fall 2019 as an assistant professor in the [Department of Public Management and Policy](https://aysps.gsu.edu/public-management-policy/). I'll be teaching classes in statistics/data science, economics, and nonprofit management in beautiful downtown Atlanta, and we'll be moving back to the South. I am so so excited about this! The Andrew Young School does amazing work in public policy, administration, and nonprofit management, and I'll be working with phenomenal colleagues and students. I still can't believe this is real.

Part of the reason I'm in shock is that for the past 2.5 years, I've been ripped apart and destroyed by the academic job market. This job market is a horrendous beast of a thing. It is soul-crushing and dream-shattering and a constant stream of rejection. While facing rejection [is good and builds grit etc., etc.](https://www.nytimes.com/2018/12/14/opinion/sunday/writers-rejections-resolutions.html), in reality it's awful.

In an effort to stay On Brand™, here are a bunch of fancy graphs and numbers showing what it's been like to apply for nearly 200 jobs since August 2016. Unlike many of my other blog posts, I haven't included any of the code to generate these. [That code](https://github.com/andrewheiss/academic-job-market/blob/master/README.Rmd) is all available in a [GitHub repository](https://github.com/andrewheiss/academic-job-market) (see `README.Rmd`), along with the [raw data](https://github.com/andrewheiss/academic-job-market/blob/master/data/jobs_clean.csv) that I've collected over the past few years (for the morbidly curious).






## Application count and outcomes



Between August 31, 2016 and November 18, 2018, I applied for 186 tenure-track and non-tenure-track academic jobs at R1 schools, liberal arts colleges, and teaching-focused public universities. I was offered one two-year visiting assistant professorship at the [Romney Institute of Public Service and Ethics](https://marriottschool.byu.edu/mpa/) at BYU (where I completed my MPA before getting my PhD), and one tenure-track assistant professorship at the Andrew Young School at Georgia State University. 

That's it. That's a 98.9% rejection rate. Here's what the weight of rejection looks like:

<img src="{{< relref "blog/2018-12-17-academic-job-market-visualized/index.markdown" >}}index_files/figure-html/plot-waffle-total-1.png" width="2100" style="display: block; margin: auto;" />

Not every one of these was an outright rejection. The typical academic search goes through a few stages:

1.  Screen hundreds of initial applications
2.  Skype or call 10-15 possible candidates
3.  Fly out ≈3 candidates
4.  Extend offer to 1 candidate

I made it through different stages of this process with many of the schools I applied to. In total, I had 27 Skype interviews and 9 flyouts over three years. This waffle plot divides up each of the applications by their final outcome (i.e. Skype, flyout, offer), discipline, and year. smh polisci.



<img src="{{< relref "blog/2018-12-17-academic-job-market-visualized/index.markdown" >}}index_files/figure-html/iron-waffles-1.png" width="2100" style="display: block; margin: auto;" />

I received my PhD in public policy, with an emphasis in political science and international relations. Many faculty at Duke emphasized that having a dual emphasis like this would be great for the academic job market—because I'm trained in both fields, I could theoretically fit comfortably in a traditional political science department or in a public policy or administration department/school. I applied to positions in both disciplines, but I was *far* less successful in getting any traction in the political science world (even though I attend and present research at ISA and APSA pretty regularly and [I have published in the Journal of Politics](https://www.andrewheiss.com/research/heiss-kelley-2017/) ¯\\\_(ツ)\_/¯). 

My first year on the market, I was split 50/50 between public administration/policy jobs and political science jobs. During my second year, because I had very little response from political science I focused almost entirely on public admin/policy. During this most recent cycle, out of desperation I went back to applying to political science jobs in international relations and comparative politics. In the chart below, my proportion of public admin/policy jobs is actually the lowest this year, but that's because (1) political science deadlines are way earlier, and (2) I essentially quit applying for jobs. This stopping was both because [my mom died suddenly](https://www.heissatopia.com/2018/11/thursday-night-and-grandmas-passing.html), which completely threw me off the rhythm of the market (i.e. I stopped applying completely until one school graciously e-mailed me to remind me to apply), and because I got an amazing job offer. I was on track for another 50/50 year, though.

<img src="{{< relref "blog/2018-12-17-academic-job-market-visualized/index.markdown" >}}index_files/figure-html/discipline-props-1.png" width="2100" style="display: block; margin: auto;" />


## Application timing

Applying for jobs is also a grueling process. Here's what a typical job needs for a complete application:

- CV
- Cover letter tailored for the job, department, and university
- Teaching statement and/or research statement and/or diversity statement (and for a few religious schools I applied to, a belief statement)
- Teaching evaluations and/or sample syllabuses
- Transcripts from graduate school
- Writing sample(s)
- 3–5 letters of recommendation

Fortunately, once I wrote a version of each of these things, applying to individual schools didn't take too terribly long. I spent August–September 2016 crafting my teaching/research/diversity statements and general cover letter, polishing my writing samples, and collecting letters of recommendation, and I edited and improved them over the whole 2.5-year process.

Writing cover letters generally took ≈45 minutes per school to scour each department's website for relevant programs, centers, faculty, and classes. On some of the days where I sent out 5+ applications, I occasionally forgot to change the recipient address in the cover letter, or writing “I am applying for job X at university Y” while leaving “University Z” from the previous application. This was always horrifying, but I got Skype interviews out of a couple of those, so I think search committees tend to be forgiving about those kinds of mistakes.

This plot shows my pace of applying for jobs. Each dot is a job; each column is a week. The fall semester has been the most intense for sending out applications. In 2016–17, my record was 16 jobs in one week; in 2017–18 it was only 6 (since I severely cut back on political science jobs); and in 2018–19, I applied for 30 jobs in one week. With a rough average of 1 hour per application, [that week was particularly intense](https://twitter.com/andrewheiss/status/1046608912617680896).

<img src="{{< relref "blog/2018-12-17-academic-job-market-visualized/index.markdown" >}}index_files/figure-html/dotplot-faux-facet-1.png" width="1800" style="display: block; margin: auto;" />

So many dots.

## Geography



Finally, another unique aspect of the academic job market is the fact that you rarely have control over where you end up. If you want to live in a specific state or city, you have to make sure a university there has an open position in your exact field (and then you have to compete against 300+ other applicants, so lolz to that). Friends, family, and neighbors would always suggest that I send my CV to nearby schools because "surely they'll find a place for you—you're smart!". But that's not how academia works.

I applied to positions in 12 different countries, with most in the United States. 

<img src="{{< relref "blog/2018-12-17-academic-job-market-visualized/index.markdown" >}}index_files/figure-html/countries-bar-1.png" width="2100" style="display: block; margin: auto;" />

Here's a world map showing the locations of all these jobs across the three years. It's really hard to see any patterns beyond the fact that I only applied for jobs in the Gulf in 2016–17, I guess?

<img src="{{< relref "blog/2018-12-17-academic-job-market-visualized/index.markdown" >}}index_files/figure-html/countries-map-cycle-1.png" width="2100" style="display: block; margin: auto;" />

Since the bulk of my applications went to schools in the US and Canada, here's a more zoomed-in map. Because there are occasionally clusters of schools—particularly along the east coast—I put a 15-mile radius around each school, and if any of those buffer zones overlapped, I increased the point size to show how many schools are in that shared area. [The code for this is actually pretty magical and ingenius](https://github.com/andrewheiss/academic-job-market/blob/master/README.Rmd#L441)—it's worth it to [check out the R code](https://github.com/andrewheiss/academic-job-market/blob/master/README.Rmd#L441) for this post just for those calculations :).

I applied to schools in 36 states + DC. I didn't apply to any schools in Alaska, Delaware, Iowa, Louisiana, Mississippi, Montana, North Dakota, Nebraska, New Hampshire, Nevada, Rhode Island, South Dakota, Vermont, West Virginia, or Wyoming.



<img src="{{< relref "blog/2018-12-17-academic-job-market-visualized/index.markdown" >}}index_files/figure-html/map-blobs-1.png" width="2400" style="display: block; margin: auto;" />

-----

Here's to being done with the job search! Off to Georgia State next year!
