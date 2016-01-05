#!/usr/bin/python
'''
Get award noms data from IMDB where it is available programmatically
Also use IMDB to add show season number and actor/actress age
  to the nominations
Save to csv for intermediate processing
'''
#Remember original point was to see if people who've won in an
#  early season become less likely to win in later seasons

import csv,mechanize,re,time,json
import numpy as np
import matplotlib.pyplot as plt

categories = ['(Actor.*T|TV Actor).*Drama',
              '(Actor.*T|TV Actor).*Comedy',
              '(Actress.*T|TV Actress).*Drama',
              '(Actress.*T|TV Actress).*Comedy',
              'Best T.*(Series|Show).*Drama',
              'Best T.*(Series|Show).*Comedy']
saveNames = ['Actor Drama','Actor Comedy','Actress Drama',
            'Actress Comedy','Show Drama','Show Comedy']

hits = {'ActorDrama':0,'ActorComedy':0,'ActressDrama':0,
        'ActressComedy':0,'SeriesDrama':0,'SeriesComedy':0}

showStartDates = {}; showEndDates = {}

br = mechanize.Browser()
response = br.open('http://www.imdb.com/awards-central/golden-globes')

yearLinks = []
for link in br.links():
    if '/event' in link.url:
        yearLinks.insert(0,link)  #so list is chronological
yearLinks = yearLinks[-45:-1]  #don't want last one

years = []
for link in yearLinks:
    yearNoms = []
    for i in range(6):
        yearNoms.append([])

    #print link.url
    year = int(link.url[link.url.find('?')-4:link.url.find('?')])
    years.append(year)

    print year
    thePage = br.follow_link(link)
    thePage = thePage.readlines()
    
    #for l in range(len(thePage)):
    #    if 'Tied' in thePage[l]:
    #        print 'check this year for ties'
    #        break

    for l in range(len(thePage)):
        for i in range(4):
            if re.search(categories[i],thePage[l]) != None:
                hits[hits.keys()[i]] += 1
                for l2 in range(l+1,l+50):
                    if re.search('^<a[^>]*>([^<]*)</a>',thePage[l2]) != None:
                        yearNoms[i].append((re.search('^<a[^>]*>([^<]*)</a>',thePage[l2]).groups()[0],
                            re.search('^>([^<]*)</a>',thePage[l2+1]).groups()[0]))
                    if '<h2>' in thePage[l2]:
                        break

        for i in [4,5]:
            if re.search(categories[i],thePage[l]) != None:
                hits[hits.keys()[i]] += 1
                for l2 in range(l+1,l+50):
                    if re.search('^<a[^>]*>([^<]*)</a>',thePage[l2]) != None:
                        yearNoms[i].append(re.search('^<a[^>]*>([^<]*)</a>',
                            thePage[l2]).groups()[0])
                        
                        #maybe need to get the start year
                        if yearNoms[i][-1] not in showStartDates.keys():
                            showLink = re.search('^<a\s*href=\"([^\"]*)\"',
                                thePage[l2]).groups()[0]
                            print showLink
                            showDetails = br.open(showLink)
                            for line in showDetails.readlines():
                                if '<title>' in line and 'TV Series' in line:
                                    yearText = re.search('TV Series ([0-9]{4}).*([0-9]{4}|\s)\)',
                                        line).groups()
                                    showStartDates[yearNoms[i][-1]] = int(yearText[0])
                                    if yearText[1] == ' ':
                                        showEndDates[yearNoms[i][-1]] = 2016
                                    else:
                                        showEndDates[yearNoms[i][-1]] = int(yearText[1])
                                    break
                                elif '<title>' in line and \
                                    ('TV Mini-Series' in line or 'TV Movie' in line):
                                    showStartDates[yearNoms[i][-1]] = \
                                        int(re.search('TV (Mini-Series|Movie) ([0-9]{4})',
                                            line).groups()[1])
                                    showEndDates[yearNoms[i][-1]] = showStartDates[yearNoms[i][-1]] + 1
                                    break
                            br.back()

                    if '<h2>' in thePage[l2]:
                        break
                #print noms

    with open('gglobesRawData.csv','a') as csvfile:
        mywriter = csv.writer(csvfile)
        mywriter.writerow([year])
        for i in range(4):
            mywriter.writerow([saveNames[i]])
            for j in range(len(yearNoms[i])):
                mywriter.writerow(yearNoms[i][j])
        for i in range(4,6):
            mywriter.writerow([saveNames[i]])
            for j in range(len(yearNoms[i])):
                mywriter.writerow([yearNoms[i][j]])

#also save dictionary of show start and end dates
with open('gglobesShowStartDates.json','w') as f:
    json.dump(showStartDates,f)
with open('gglobesShowEndDates.json','w') as f:
    json.dump(showEndDates,f)


print hits.values()