---
layout: page
title: 📚 Resources
description: Useful links and resources.
nav_order: 4
---

# 📚 Resources 
{:.no_toc}

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}


## Course Texts

As this course has been created and tailored for the specific needs of our Data Science majors,
there is not one single textbook that fits the course. As such, we will be utilizing excerpts
from a variety of textbooks / course notes. The specific chapters/sections mapping to each lecture are linked
on the main page, but here are each of the texts in their entirety. You are not responsible
for anything that is in these texts that is not covered in lecture; this is here for your
reference only.

### Textbooks

- [ModernDive](https://moderndive.com/v2/) - Chester Ismay, Albert Kim and Arturo Valdivia
- [Handbook of Regression Modeling in People Analytics](https://peopleanalytics-regression-book.org/) - Keith McNulty
- [Time Series Analysis With R](https://nicolarighetti.github.io/Time-Series-Analysis-With-R/index.html) - Nicola Righetti
- [R Cookbook](https://rc2e.com/) - James (JD) Long, Paul Teetor


### Course notes

- [PSU Applied Statistics Course Notes](https://online.stat.psu.edu/stat500/) - Penn State Department of Statistics
- [Statistics and Predictive Analyics Course Notes](https://bookdown.org/jefftemplewebb/IS-6489/) - Jeff Webb, University of Utah
- [Data Handling, Visualization and Statistics Course Notes](https://jonesor.github.io/BB852_Book/) - Owen R. Jones, University of Southern Denmark


## R, RStudio and R Markdown
In this course, we will be using R, in the RStudio environment. To do so, you will need to install both R and RStudio onto your machines. You can do both of these things by going here: 

 - [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)

Next, you will be asked to submit many assignments in this class using R Markdown. Specifically, we will want pdf output files.

 - If you already have a LaTeX compiler on your computer, RStudio should detect it and it should automatically work. If you are not sure whether you do or not, then you can just try it. To do so,

1. In RStudio, go to File -> New File -> R Markdown
2. Then select "PDF" as the Default Output Format, and hit OK
3. At the top of the window, hit the "Knit" button next to the blue ball of yarn. If this produces a pdf file, then you are set!

 - If that didn't work or you know that you do not have a LaTeX compiler already on your computer, the easiest thing to do is to install the `tinytex` package. To do this, just type these two lines in the Console in RStudio (one after the other):

`install.packages("tinytex")`

`tinytex::install_tinytex()`

After this, you should be able to "knit" an Rmd file to a pdf following the steps above. If not, please come seek help from us!


## UC San Diego Links
- [Data Science Undergraduate Program](https://datascience.ucsd.edu/undergraduate/)
- [Data Science Advising](https://datascience.ucsd.edu/current-students/academic-advising/)
- [Library Guide to Data and Statistics](https://ucsd.libguides.com/data-statistics)
- [Academic Integrity Homepage](https://academicintegrity.ucsd.edu)
- [Counseling and Psychological Services (CAPS)](https://caps.ucsd.edu)

## Finding Datasets for Personal Projects
- [Where to find datasets](https://rampure.org/find-datasets)
