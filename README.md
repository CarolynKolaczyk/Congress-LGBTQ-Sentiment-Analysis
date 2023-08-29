# Sentiment Analysis of LGBTQ Issues in US Congressional Speeches
This project was originally completed for a Text as Data course. The goal of this project is to evaluate the sentiment of US Congressional speeches discussing LGBTQ issues, and investigate whether there is a relationship between speech sentiment and the passing of LGBTQ legislation, specifically the legalization of same-sex marriage at the state level. It also explores the effect of speakers’ state and political party on the sentiment of LGBTQ related speeches.

## Data
### hein-daily
* Dataset of textual data from the “Congress Record for the 43rd-114th Congresses: Parsed Speeches and Phrase Counts”
* Distributed by the [Stanford Libraries Social Science Data Collection](https://data.stanford.edu/)
* Contains all text spoken on the floor of both the US House of Representatives and the US Senate, from the 97th to 114th Congresses (1873 to 2017), as well as metadata about each speech and speaker.

### negative-words
Data dictionary of negative words from [Hu and Liu (2004)](https://www.cs.uic.edu/~liub/publications/kdd04-revSummary.pdf)

### positive-words
Data dictionary of positive words from [Hu and Liu (2004)](https://www.cs.uic.edu/~liub/publications/kdd04-revSummary.pdf)

### marriage_law_dates
Same-sex marriage legalization dates returned from Google, for states who legalized prior to the 2015 Supreme Court ruling in *Obergefell v. Hodges*.

### Data Processing
Each session has a 3-digit code eg. the 97th Congress has code 097. For each session, we merge data from 3 files: descr_###.txt, speeches_###.txt, and ###_SpeakerMap.txt. We perform an inner join on these 3 files on variable 'speech_id', then select the following variables to use in our analysis:
* 'speakerid': unique identifier for speaker
* 'speech_id': unique identifier for speech
* 'state': speaker state
* 'party': speaker political party
* 'date': date of speech
* 'speech': text of speech-data

We repeat this for all sessions.

## Sentiment Analysis
(add)
