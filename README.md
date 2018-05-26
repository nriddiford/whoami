[![whoami](https://img.shields.io/badge/launch-whoami-brightgreen.svg)](https://whatstat.shinyapps.io/whoami/)

# whoami

A tool to perform sentiment analysis on all of you sent emails (from a Gmail account).

[Click here](https://whatstat.shinyapps.io/whoami/) to run the shiny web app.

Please [e-mail](mailto:nickriddiford@gmail.com) me if you are experiencing problems with whoami.


## Download whoami scripts

```
git clone https://github.com/nriddiford/whoami.git
```

## Install requirements for python code

```
cd whoami
pip install -r dependencies.txt
```

## Download all sent emails (replace your_email@gmail.com with your email address)

```
python imapy.py -e your_email@gmail.com
```

You will be asked for your password, and then the script will begin to save emails to a .tsv file called `emails.tsv` in the same directory (this can be changed using the `-o` flag).

A message will be printed every 100 messages. Dependeing on the number of sent messages this might take some time (~ 250 messages a minute).

This file `emails.tsv` can then be used as input for the whoami shiny web app.

[![whoami](https://img.shields.io/badge/launch-whoami-brightgreen.svg)](https://whatstat.shinyapps.io/whoami/)
