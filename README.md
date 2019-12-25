# TED Talk Transcripts
## Project Description
The importance of public speaking as a skill is well documented, but it is still an unpleasant task for most people. In this context, we set out to analyze a series of TED Talk transcripts in hopes of uncovering insights regarding the best practices regarding public speaking.  

## Data Source
The 2,467 TED Talk audio recordings have been scraped from the official TED website and have been downloaded as a csv file from Kaggle. The dataset includes duration, film date, languages, speaker, tags, title, URL, views, and transcripts. The dates of the talks range from 1984 to 2017.

## Analysis
We will examine the following relationships with sentiment correlation and audience response classification:
1)  Audience response (such as applause and laughter) and transcript content
2)  Popularity (measured as the number of views and the rating) and transcript content

## Challenges
1)  The sentiment will change throughout each transcript. Therefore, we will break down each transcript into sentences. Selecting the optimal way to break up the sentences will be challenging because each sentence ends in a different way.
2)  Audience responses will differ based on the mood of the talk, but the mood of the talk will not necessarily be indicative of the quality of the talk. In other words, a lack of applause or laughter may not be representative of the talk’s popularity if the tone and subject of the talk are serious. (For example, one would not expect much laughter during a talk about the current opioid crisis. In this case, it would be inappropriate to take laughter into consideration for our analysis.)  
3)  We will need to keep in mind that our transcripts do not indicate the degree to which an audience laughs or applauds; the data only indicate if these reactions occurred or if they did not.

