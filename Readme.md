# Proposal for Semester Project


<!-- 
Please render a pdf version of this Markdown document with the command below (in your bash terminal) and push this file to Github

quarto render Readme.md --to pdf
-->

**Patterns & Trends in Environmental Data / Computational Movement
Analysis Geo 880**

| Semester:      | FS24                                     |
|:---------------|:---------------------------------------- |
| **Data:**      | Mountainbike activity data (Strava & Komoot) |
| **Title:**     | Have I gotten faster? - An analysis of Mountainbiking data                |
| **Student 1:** | Annika Hirsch                            |
| **Student 2:** | Name of Student 2                        |

## Abstract 
<!-- (50-60 words) -->

While Mountainbiking I often repeat the same trails. Since I hope my technique has improved over the years, I wonder if that is also represented in the times. Have I gotten faster, smoother, need less rests? For comparability I will probably only analyze data from one specific trail and do primarily analyze the descent, since that is my own preference.  

## Research Questions
<!-- (50-60 words) -->

Have I gotten faster over the years?
In what parts have I improved most?
Is my speed linked to landscape properties?
What would be my ideal time of the segment, piecing together all the fastest times of the sub segments?
Is there a difference in stopping because of falls, waiting for hikers or just resting?

## Results / products
<!-- What do you expect, anticipate? -->

I know that my fastest time on the trail segment has gotten faster and I also know that I can now ride more sections compared to earlier records, where I had to get off the bike and walk. But I wonder how that is represented in the characteristics of the activity records. Has my time gotten faster in specific sections, has my overall speed improved, can I keep a constant speed or do I just rest less? While I know that I am generally faster in easy sections compared to more technical sections, I do not know how that is represented in the speed or smoothness. 

## Data
<!-- What data will you use? Will you require additional context data? Where do you get this data from? Do you already have all the data? -->

The data that I will be using are multiple GPS tracked activities while Mountainbiking. The trails I ride in my home regions do not vary too much, therefore I have usually a few activity recordings per trail. Since I am mainly interested in the downhill part and less about the uphill, the track that I choose is a Trail in the "Bleikiwald" on the "Harder" in Interlaken. It is about 1,5 km long and includes a descent of approximately 320 m and currently my fastest time is 8:08 minutes. 

Additionally I could use a Digital Elevation Model (DEM) to correlate the movement to the slope. The specific DEM that I would be using is swissALTI3D with the spatial resolution of 0.5 m. For the visualization of my results I will probably use some form of aerial image. 

## Analytical concepts
<!-- Which analytical concepts will you use? What conceptual movement spaces and respective modelling approaches of trajectories will you be using? What additional spatial analysis methods will you be using? -->

The most important things that i will be analyzing are speed and smoothness. These two variables only require the trajectories themselves. Considering that the trail that I am riding on is quite well defined and narrow and the accuracy of the GPS is less than the width of the trail, it might be good to match the GPS fixes to the trail. 

I might superimpose the trajectories (or just the trail, as the GPS signal might be off a few meters, which can make a big difference for the altitude) over a DEM to match the variables speed and smoothness to the slope to see if there is a correlation. A different approach of testing a for correlation would be to manually evaluate the difficulty of the trail, and checking whether the speed and smoothness match this classification. 

## R concepts
<!-- Which R concepts, functions, packages will you mainly use. What additional spatial analysis methods will you be using? -->

I am planning on analyzing the speed and the smoothness of the mountainbike rides. I will use the "sf" package as well as "tidyverse" ("ggplot2", "dplyr", etc.) for the data preparation or visualization. To analyze the similarity of the different activity records I will use "SimilarityMeasures". For the visualization of my results, additionally to "ggplot2", I might use "leaflet" or "tmap". 

## Risk analysis
<!-- What could be the biggest challenges/problems you might face? What is your plan B? -->

The biggest problem currently, is that my plan of what exactly I am going to do and analyze still is very vague. So I have to do some evaluations beforehand to see what is possible and what I am able to analyze. I will have to define and adjust the exact research question(s) accordingly. 

## Questions? 
<!-- Which questions would you like to discuss at the coaching session? -->

I would like some inputs from you, in which direction I should lead my project. 

I have some other questions: 
Is the correlation of the movement itself with landscape properties still in the scope of the semester project?
I do not know yet how I will analyze the speed and smoothness or the breaks (rests, falls etc) exactly. So any guidance would be appreciated. 




