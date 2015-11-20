#!/usr/bin/python
'''
Get award noms data from IMDB where it is available programmatically
'''

import csv,mechanize,re

categories = ['Actor.*Television.*Drama',
              'Actor.*Television.*Comedy',
              'Actress.*Television.*Drama',
              'Actress.*Television.*Comedy',
              'Best T.*Series.*Drama',
              'Best T.*Series.*Comedy']
hits = {'ActorDrama':0,'ActorComedy':0,'ActressDrama':0,
        'ActressComedy':0,'SeriesDrama':0,'SeriesComedy':0}

showNoms = {}; showWins = {}
personNoms = {}; personWins = {}

br = mechanize.Browser()
response = br.open('http://www.imdb.com/awards-central/golden-globes')

#**go chronologically so that showNoms contains the total number of prior
#    nominations in a particular year.

years = []
for link in br.links():
    if '/event' in link.url:
        #print link.url
        year = link.url[link.url.find('?')-4:link.url.find('?')]
        years.append(int(year))

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
                    print noms
                    for s,p in noms:
                        if p in personNoms.keys():
                            personNoms[p] += 1
                        else:
                            personNoms[p] = 1
                    if noms[0][1] in personWins.keys():
                        personWins[noms[0][1]] += 1
                    else:
                        personWins[noms[0][1]] = 1
            for i in [4,5]:
                noms = []
                if re.search(categories[i],thePage[l]) != None:
                    hits[hits.keys()[i]] += 1
                    for l2 in range(l+1,l+50):
                        if re.search('^<a[^>]*>([^<]*)</a>',thePage[l2]) != None:
                            noms.append(re.search('^<a[^>]*>([^<]*)</a>',thePage[l2]).groups()[0])
                        if '<h2>' in thePage[l2]:
                            break
                    #print noms
                    for s in noms:
                        if s in showNoms.keys():
                            showNoms[s] += 1
                        else:
                            showNoms[s] = 1
                    if noms[0] in showWins.keys():
                        showWins[noms[0]] += 1
                    else:
                        showWins[noms[0]] = 1
    if len(years) > 30:
        print 'stopping'
        break
print hits.values()
