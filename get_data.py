#!/usr/bin/python
'''
Get award noms data from IMDB where it is available programmatically
Also use IMDB to add show season number and actor/actress age
  to the nominations
Save diagnostics of interest to csv in convenient form for d3.js plot
'''
#Remember original point was to see if people who've won in an
#  early season become less likely to win in later seasons

import csv,mechanize,re,time
import numpy as np
import matplotlib.pyplot as plt

categories = ['(Actor.*T|TV Actor).*Drama',
              '(Actor.*T|TV Actor).*Comedy',
              '(Actress.*T|TV Actress).*Drama',
              '(Actress.*T|TV Actress).*Comedy',
              'Best T.*(Series|Show).*Drama',
              'Best T.*(Series|Show).*Comedy']
hits = {'ActorDrama':0,'ActorComedy':0,'ActressDrama':0,
        'ActressComedy':0,'SeriesDrama':0,'SeriesComedy':0}

showNoms = {}; showWins = {}
personNoms = {}; personWins = {}

winWithPrevNoms = dict(zip(range(15),[0]*15))
loseWithPrevNoms = dict(zip(range(15),[0]*15))
winWithPrevWins = dict(zip(range(10),[0]*10))
loseWithPrevWins = dict(zip(range(10),[0]*10))
winBySeason = dict(zip(range(1,16),[0]*15))
loseBySeason = dict(zip(range(1,16),[0]*15))
winByRunPt = dict()
loseByRunPt = dict()

showStartDates = {}; showEndDates = {}

def updateHistories(noms,prevNoms,winWithPrevNoms,loseWithPrevNoms):
    if noms[0] in prevNoms.keys():
        winWithPrevNoms[prevNoms[noms[0]]] += 1
    else:
        winWithPrevNoms[0] += 1
    for p in noms[1:]:
        if p in prevNoms.keys():
            loseWithPrevNoms[prevNoms[p]] += 1
        else:
            loseWithPrevNoms[0] += 1
    
    return winWithPrevNoms,loseWithPrevNoms

def updateNoms(noms,prevNoms,prevWins):
    for p in noms:
        if p in prevNoms.keys():
            prevNoms[p] += 1
        else:
            prevNoms[p] = 1
    if noms[0] in prevWins.keys():
        prevWins[noms[0]] += 1
    else:
        prevWins[noms[0]] = 1

br = mechanize.Browser()
response = br.open('http://www.imdb.com/awards-central/golden-globes')

yearLinks = []
for link in br.links():
    if '/event' in link.url:
        yearLinks.insert(0,link)  #so list is chronological
yearLinks = yearLinks[-45:]

years = []
for link in yearLinks:
    #print link.url
    year = int(link.url[link.url.find('?')-4:link.url.find('?')])
    years.append(year)

    print year
    thePage = br.follow_link(link)
    thePage = thePage.readlines()
    for l in range(len(thePage)):
        for i in range(4):
            noms = []
            if re.search(categories[i],thePage[l]) != None:
                hits[hits.keys()[i]] += 1
                for l2 in range(l+1,l+50):
                    if re.search('^<a[^>]*>([^<]*)</a>',thePage[l2]) != None:
                        noms.append((re.search('^<a[^>]*>([^<]*)</a>',thePage[l2]).groups()[0],
                            re.search('^>([^<]*)</a>',thePage[l2+1]).groups()[0]))
                    if '<h2>' in thePage[l2]:
                        break
                
                #record wins and losses as fn of previous total noms
                #assuming people are only nominated for one show ever(?)
                winWithPrevNoms,loseWithPrevNoms = \
                    updateHistories([noms[i][1] for i in range(len(noms))],
                        personNoms,winWithPrevNoms,loseWithPrevNoms)

                #record wins and losses as fn of previous total wins
                winWithPrevWins,loseWithPrevWins = \
                    updateHistories([noms[i][1] for i in range(len(noms))],
                        personWins,winWithPrevWins,loseWithPrevWins)

                #update noms and wins
                updateNoms([noms[i][1] for i in range(len(noms))],
                    personNoms,personWins)
                
        for i in [4,5]:
            noms = []
            if re.search(categories[i],thePage[l]) != None:
                hits[hits.keys()[i]] += 1
                for l2 in range(l+1,l+50):
                    if re.search('^<a[^>]*>([^<]*)</a>',thePage[l2]) != None:
                        noms.append(re.search('^<a[^>]*>([^<]*)</a>',
                            thePage[l2]).groups()[0])
                        
                        #maybe need to get the start year
                        if noms[-1] not in showStartDates.keys():
                            showLink = re.search('^<a\s*href=\"([^\"]*)\"',
                                thePage[l2]).groups()[0]
                            print showLink
                            showDetails = br.open(showLink)
                            for line in showDetails.readlines():
                                if '<title>' in line and 'TV Series' in line:
                                    yearText = re.search('TV Series ([0-9]{4}).*([0-9]{4}|\s)\)',
                                        line).groups()
                                    showStartDates[noms[-1]] = int(yearText[0])
                                    if yearText[1] == ' ':
                                        showEndDates[noms[-1]] = 2016
                                    else:
                                        showEndDates[noms[-1]] = int(yearText[1])
                                    break
                                elif '<title>' in line and \
                                    ('TV Mini-Series' in line or 'TV Movie' in line):
                                    showStartDates[noms[-1]] = \
                                        int(re.search('TV (Mini-Series|Movie) ([0-9]{4})',
                                            line).groups()[1])
                                    showEndDates[noms[-1]] = showStartDates[noms[-1]] + 1
                                    break
                            br.back()

                    if '<h2>' in thePage[l2]:
                        break
                #print noms

                #record wins and losses as fn of previous total noms
                winWithPrevNoms,loseWithPrevNoms = \
                    updateHistories(noms,showNoms,
                        winWithPrevNoms,loseWithPrevNoms)

                #record wins and losses as fn of previous total noms
                winWithPrevWins,loseWithPrevWins = \
                    updateHistories(noms,showWins,
                        winWithPrevWins,loseWithPrevWins)

                #update noms and wins
                updateNoms(noms,showNoms,showWins)

                #record wins and losses as fn of season
                thisSeason = year - showStartDates[noms[0]]
                winBySeason[thisSeason] += 1
                for s in noms[1:]:
                    thisSeason = year - showStartDates[s]
                    loseBySeason[thisSeason] += 1

                #and as fn of point in total run
                pointInRun = (year-showStartDates[noms[0]])/\
                    float(showEndDates[noms[0]]-showStartDates[noms[0]]+1)
                if round(pointInRun,3) not in winByRunPt.keys():
                    winByRunPt[round(pointInRun,3)] = 1
                else:
                    winByRunPt[round(pointInRun,3)] += 1
                for s in noms[1:]:
                    pointInRun = (year-showStartDates[s])/\
                        float(showEndDates[s]-showStartDates[s]+1)
                    if round(pointInRun,3) not in loseByRunPt.keys():
                        loseByRunPt[round(pointInRun,3)] = 1
                    else:
                        loseByRunPt[round(pointInRun,3)] += 1

print hits.values()

#Can plot histogram of winWithPrevNoms, but should be weighted somehow
#  by most common number of prevNoms, which is winWithPrevNoms+loseWithPrevNoms
#Or maybe it should be histogram of winWithPrevNoms/(totalWithPrevNoms)
prevNomsOrWins = range(15)
winFracPrevWins = [0]*10
winFracPrevNoms = [0]*15
winFracBySeason = [0]*15
for i in range(10):
    if winWithPrevWins[i]+loseWithPrevWins[i] > 0:
        winFracPrevWins[i] = winWithPrevWins[i] / \
            float(winWithPrevWins[i]+loseWithPrevWins[i])
    else:
        winFracPrevWins[i] = np.nan

for i in range(15):
    if winWithPrevNoms[i]+loseWithPrevNoms[i] > 0:
        winFracPrevNoms[i] = winWithPrevNoms[i] / \
            float(winWithPrevNoms[i]+loseWithPrevNoms[i])
    else:
        winFracPrevNoms[i] = np.nan

for i in range(1,13):
    if winBySeason[i]+loseBySeason[i] > 0:
        winFracBySeason[i] = winBySeason[i] / \
            float(winBySeason[i]+loseBySeason[i])
    else:
        winFracBySeason[i] = np.nan

plt.plot(prevNomsOrWins,winFracPrevNoms,'ks',ms=10)
plt.xlim([-0.5,9.5])
plt.xlabel('Previous Noms'); plt.ylabel('Win Fraction')
plt.show()
#may rather count previous losing noms

plt.plot(prevNomsOrWins[:10],winFracPrevWins,'ks',ms=10)
plt.xlim([-0.5,9.5])
plt.xlabel('Previous Wins'); plt.ylabel('Win Fraction')
plt.show()

#Also to do by show season, scraped from IMDB.
#Instead of scaling by total noms for that season, scale
#  by point in the show's run?
plt.plot(range(1,16),winBySeason.values(),'ks',ms=10)
plt.xlim([0.5,12.5])
plt.xlabel('Season'); plt.ylabel('Win Fraction')
plt.show()

#write to file
if len(years) >= 3 and \
    [hits.values()[i] == len(years) for i in range(6)]:
    with open('%i-%i_winFracPrevNoms.csv' % (years[0],years[-1]),
        'w') as csvfile:
        mywriter = csv.writer(csvfile)
        mywriter.writerow(['prevNoms','numWins','numTot','winFrac'])
        for i in range(len(winFracPrevNoms)):
            mywriter.writerow([prevNomsOrWins[i],winWithPrevNoms[i],
                winWithPrevNoms[i]+loseWithPrevNoms[i],
                winFracPrevNoms[i]])

    with open('%i-%i_winFracPrevWins.csv' % (years[0],years[-1]),
        'w') as csvfile:
        mywriter = csv.writer(csvfile)
        mywriter.writerow(['prevWins','numWins','numTot','winFrac'])
        for i in range(len(winFracPrevWins)):
            mywriter.writerow([prevNomsOrWins[i],winWithPrevWins[i],
                winWithPrevWins[i]+loseWithPrevWins[i],
                winFracPrevWins[i]])

    with open('%i-%i_winBySeason.csv' % (years[0],years[-1]),
        'w') as csvfile:
        mywriter = csv.writer(csvfile)
        mywriter.writerow(['season','numWins','numTot','winFrac'])
        for i in range(len(winBySeason)):
            mywriter.writerow([i+1,winBySeason[i+1],
                winBySeason[i+1]+loseBySeason[i+1],
                winFracBySeason[i]])


