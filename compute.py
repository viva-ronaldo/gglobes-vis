#!/usr/bin/python
'''
Process list of nominations and 
save diagnostics of interest to csv in convenient form for d3.js plot
'''
#Remember original point was to see if people who've won in an
#  early season become less likely to win in later seasons

#**manually add tie cases where noms[1] also wins: Actor Drama 76, Actress Comedy 74,
#  Actor Comedy 78, Show Comedy 80, Actress Drama 82, Actress Comedy 86, 
#check 88,89,91,95,99

import csv,re,time,json
import numpy as np
import matplotlib.pyplot as plt

saveNames = ['Actor Drama','Actor Comedy','Actress Drama',
            'Actress Comedy','Show Drama','Show Comedy']              
hits = {'ActorDrama':0,'ActorComedy':0,'ActressDrama':0,
        'ActressComedy':0,'SeriesDrama':0,'SeriesComedy':0}

showNoms = {}; showWins = {}; showLosses = {}
personNoms = {}; personWins = {}; personLosses = {}

winWithPrevNoms = dict(zip(range(15),[0]*15))
loseWithPrevNoms = dict(zip(range(15),[0]*15))
winWithPrevWins = dict(zip(range(10),[0]*10))
loseWithPrevWins = dict(zip(range(10),[0]*10))
winWithPrevLosses = dict(zip(range(15),[0]*15))
loseWithPrevLosses = dict(zip(range(15),[0]*15))
winBySeason = dict(zip(range(1,16),[0]*15))
loseBySeason = dict(zip(range(1,16),[0]*15))
winByRunPt = dict()
loseByRunPt = dict()

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

def updateNoms(noms,prevNoms,prevWins,prevLosses):
    for p in noms:
        if p in prevNoms.keys():
            prevNoms[p] += 1
        else:
            prevNoms[p] = 1

    if noms[0] in prevWins.keys():
        prevWins[noms[0]] += 1
    else:
        prevWins[noms[0]] = 1

    for p in noms[1:]:
        if p in prevLosses.keys():
            prevLosses[p] += 1
        else:
            prevLosses[p] = 1


#-----

with open('gglobesShowStartDates.json','r') as f:
    showStartDates = json.load(f)
with open('gglobesShowEndDates.json','r') as f:
    showEndDates = json.load(f)

years = []
with open('gglobesRawData.csv','r') as csvfile:
    myreader = csv.reader(csvfile)
    count = 0
    for row in myreader:
        try:
            if 1960 < int(row[0]) < 2020:
                year = int(row[0])
                count = 0
                noms = []
        except:
            pass

        if count == 0:
            years.append(year)
        elif row[0] in saveNames:
            noms = []
        else:
            if len(row) == 1:
                noms.append(row[0])
            else:
                noms.append((row[0],row[1]))

        if len(noms) == 5:
            #print noms

            if type(noms[0]) == type(''):
                #record wins and losses as fn of previous total noms
                winWithPrevNoms,loseWithPrevNoms = \
                    updateHistories(noms,showNoms,
                        winWithPrevNoms,loseWithPrevNoms)

                #record wins and losses as fn of previous total noms
                winWithPrevWins,loseWithPrevWins = \
                    updateHistories(noms,showWins,
                        winWithPrevWins,loseWithPrevWins)

                winWithPrevLosses,loseWithPrevLosses = \
                    updateHistories(noms,showLosses,
                        winWithPrevLosses,loseWithPrevLosses)

                #update noms and wins
                updateNoms(noms,showNoms,showWins,showLosses)

                #record wins and losses as fn of season
                thisSeason = year - showStartDates[noms[0]]
                if thisSeason > 5:
                    print noms[0],thisSeason,year
                winBySeason[thisSeason] += 1
                for s in noms[1:]:
                    thisSeason = year - showStartDates[s]
                    loseBySeason[thisSeason] += 1

                #and as fn of point in total run
                pointInRun = (year-showStartDates[noms[0]])/\
                    float(showEndDates[noms[0]]-showStartDates[noms[0]]+1)
                if pointInRun > 1:
                    pass
                else:
                    if pointInRun == 1.0:
                        print noms[0],' won in last season(?)'
                    if round(pointInRun,1) not in winByRunPt.keys():
                        winByRunPt[round(pointInRun,1)] = 1
                    else:
                        winByRunPt[round(pointInRun,1)] += 1
                for s in noms[1:]:
                    pointInRun = (year-showStartDates[s])/\
                        float(showEndDates[s]-showStartDates[s]+1)
                    if pointInRun > 1:
                        pass
                        #**breaking for The Office currently
                    else:
                        if round(pointInRun,1) not in loseByRunPt.keys():
                            loseByRunPt[round(pointInRun,1)] = 1
                        else:
                            loseByRunPt[round(pointInRun,1)] += 1

            else:
                #record wins and losses as fn of previous total noms
                winWithPrevNoms,loseWithPrevNoms = \
                    updateHistories(noms,personNoms,
                        winWithPrevNoms,loseWithPrevNoms)

                #record wins and losses as fn of previous total noms
                winWithPrevWins,loseWithPrevWins = \
                    updateHistories(noms,personWins,
                        winWithPrevWins,loseWithPrevWins)

                winWithPrevLosses,loseWithPrevLosses = \
                    updateHistories(noms,personLosses,
                        winWithPrevLosses,loseWithPrevLosses)

                updateNoms(noms,personNoms,personWins,personLosses)

            noms = []


        count += 1

print sum(winWithPrevNoms.values())+sum(loseWithPrevNoms.values())
print sum(winWithPrevWins.values())+sum(loseWithPrevWins.values())
print sum(winWithPrevLosses.values())+sum(loseWithPrevLosses.values())

#Can plot histogram of winWithPrevNoms, but should be weighted somehow
#  by most common number of prevNoms, which is winWithPrevNoms+loseWithPrevNoms
#Or maybe it should be histogram of winWithPrevNoms/(totalWithPrevNoms)
prevNomsOrWins = range(15)
winFracPrevWins = [0]*10
winFracPrevNoms = [0]*15
winFracPrevLosses = [0]*15
winFracBySeason = [0]*15
winFracByRunPt = [0]*len(loseByRunPt.keys())
print 'wins,losses for each prev wins'
for i in range(10):
    if winWithPrevWins[i]+loseWithPrevWins[i] > 0:
        winFracPrevWins[i] = winWithPrevWins[i] / \
            float(winWithPrevWins[i]+loseWithPrevWins[i])
        print i,winWithPrevWins[i],loseWithPrevWins[i]
    else:
        winFracPrevWins[i] = np.nan

print 'wins,losses for each prev noms'
for i in range(15):
    if winWithPrevNoms[i]+loseWithPrevNoms[i] > 0:
        winFracPrevNoms[i] = winWithPrevNoms[i] / \
            float(winWithPrevNoms[i]+loseWithPrevNoms[i])
        print i,winWithPrevNoms[i],loseWithPrevNoms[i]
    else:
        winFracPrevNoms[i] = np.nan

print 'wins,losses for each prev losses'
for i in range(15):
    if winWithPrevLosses[i]+loseWithPrevLosses[i] > 0:
        winFracPrevLosses[i] = winWithPrevLosses[i] / \
            float(winWithPrevLosses[i]+loseWithPrevLosses[i])
        print i,winWithPrevLosses[i],loseWithPrevLosses[i]
    else:
        winFracPrevLosses[i] = np.nan

for i in range(1,13):
    if winBySeason[i]+loseBySeason[i] > 0:
        winFracBySeason[i-1] = winBySeason[i] / \
            float(winBySeason[i]+loseBySeason[i])
    else:
        winFracBySeason[i-1] = np.nan

count = 0
for rp in loseByRunPt.keys():
    if rp not in winByRunPt.keys():
        winByRunPt[rp] = 0
    if winByRunPt[rp]+loseByRunPt[rp] > 0:
        winFracByRunPt[count] = winByRunPt[rp] / \
            float(winByRunPt[rp]+loseByRunPt[rp])
    else:
        winFracByRunPt[count] = np.nan
    count += 1
#TODO: check. seems to show higher fraction for runPt=1.0, i.e. final year
#though only 6 cases (Office (UK), Extras, Breaking Bad)
#Get some better way of categorising, maybe as years from end

plt.plot(prevNomsOrWins,winFracPrevNoms,'ks',ms=10)
plt.xlim([-0.5,9.5])
plt.xlabel('Previous Noms'); plt.ylabel('Win Fraction')
plt.show()
#only reliable up to 4

#plt.plot(prevNomsOrWins[:10],winFracPrevWins,'ks',ms=10)
#plt.xlim([-0.5,9.5])
#plt.xlabel('Previous Wins'); plt.ylabel('Win Fraction')
#plt.show()
#only reliable up to ~6

#plt.plot(prevNomsOrWins,winFracPrevLosses,'ks',ms=10)
#plt.xlim([-0.5,9.5])
#plt.xlabel('Previous Losses'); plt.ylabel('Win Fraction')
#plt.show()
#only reliable up to 4

#Also to do by show season, scraped from IMDB.
#Instead of scaling by total noms for that season, scale
#  by point in the show's run?
showLengths = {}
for key in showStartDates.keys():
    length = showEndDates[key] - showStartDates[key] + 1
    if length > 15:
        print key,length,showStartDates[key],showEndDates[key]
    if length in showLengths.keys():
        showLengths[length] += 1
    else:
        showLengths[length] = 1
runLengthFracs = [0]*15
for l in range(1,16):
    if l in showLengths.keys():
        runLengthFracs[l-1] = 100 * showLengths[l] / np.float(sum(showLengths.values()))
    else:
        runLengthFracs[l-1] = 0.    
plt.plot(range(1,16),winBySeason.values(),'ks',ms=10)
plt.plot(range(1,16),runLengthFracs,'g--',lw=2,label='Total run length')
plt.xlim([0.5,12.5])
plt.xlabel('Season'); plt.ylabel('Fraction')
plt.legend(loc='upper right')
plt.title('Show wins by season')
plt.show()
#can label the 5 cases >s5




#write to file
if 1 and len(years) >= 3 and \
    [hits.values()[i] == len(years) for i in range(6)]:
    with open('%i-%i_winFracPrevNoms.csv' % (years[0],years[-1]),
        'w') as csvfile:
        mywriter = csv.writer(csvfile)
        mywriter.writerow(['prevNoms','numWins','numTot','winFrac'])
        for i in range(10):
            mywriter.writerow([prevNomsOrWins[i],winWithPrevNoms[i],
                winWithPrevNoms[i]+loseWithPrevNoms[i],
                winFracPrevNoms[i]])

    with open('%i-%i_winFracPrevWins.csv' % (years[0],years[-1]),
        'w') as csvfile:
        mywriter = csv.writer(csvfile)
        mywriter.writerow(['prevWins','numWins','numTot','winFrac'])
        for i in range(6):
            mywriter.writerow([prevNomsOrWins[i],winWithPrevWins[i],
                winWithPrevWins[i]+loseWithPrevWins[i],
                winFracPrevWins[i]])

    with open('%i-%i_winFracPrevLosses.csv' % (years[0],years[-1]),
        'w') as csvfile:
        mywriter = csv.writer(csvfile)
        mywriter.writerow(['prevLosses','numWins','numTot','winFrac'])
        for i in range(8):
            mywriter.writerow([prevNomsOrWins[i],winWithPrevLosses[i],
                winWithPrevLosses[i]+loseWithPrevLosses[i],
                winFracPrevLosses[i]])

    with open('%i-%i_winBySeason.csv' % (years[0],years[-1]),
        'w') as csvfile:
        mywriter = csv.writer(csvfile)
        mywriter.writerow(['season','numWins','numTot','winFrac','fracShows'])
        for i in range(1,12):
            mywriter.writerow([i,winBySeason[i],
                winBySeason[i]+loseBySeason[i],
                winFracBySeason[i-1],runLengthFracs[i-1]])


