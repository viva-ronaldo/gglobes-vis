from bs4 import BeautifulSoup
import requests, csv

r = requests.get('https://en.wikipedia.org/wiki/List_of_Primetime_Emmy_Award_winners', headers=headers)
soup = BeautifulSoup(r.text, 'html.parser')

#year_titles = [r for r in soup.find_all(name='th') if r.find('a')]
#year_titles = [r.find('a').get_text() for r in year_titles]
#year_titles = [int(r) for r in year_titles if r[0] in ['1','2']]

#order of columns is show comedy, show drama, show variety,
#actor comedy, actor drama, actress comedy, actress drama
#Jump to 1971 to get the current format and cover the years we need,
#  which is the first mention of All in the Family

nom_cells = [r for r in soup.find_all(name='td') if r.find('a') or r.find('i')]

nom_rowspans = [r.get('rowspan') for r in nom_cells]
nom_rowspans = [1 if r is None else int(r) for r in nom_rowspans]

nom_cells = [r.find('a').get_text() if r.find('a') is not None else r.find('i').get_text() for r in nom_cells]
start_i, end_i = 0, 0
for i in range(len(nom_cells)):
    if 'All in the Family' in nom_cells[i] and start_i == 0:
        start_i = i
    if nom_cells[i] == 'Jodie Comer':
        end_i = i+1
        break
nom_cells = nom_cells[start_i:end_i]  #this is 1971 to 2019
nom_rowspans = nom_rowspans[start_i:end_i]  #all columns start new in 1971

categories = ['Show Comedy','Show Drama','Show Variety','Actor Comedy','Actor Drama','Actress Comedy','Actress Drama']
current_values = ['']*7
rowspan_remaining = [0]*7
n_index = 0

with open('data/emmy_winners_1971to2019.csv','w') as file:
    mywriter = csv.writer(file)
    mywriter.writerow(['category','year','winner'])

    for s in range((2019-1971+1)*7):
        col_index = s % 7
        current_year = 1971 + s // 7

        if rowspan_remaining[col_index] == 0:
            #print(current_year, categories[col_index], nom_cells[n_index])
            mywriter.writerow([categories[col_index], current_year, nom_cells[n_index]])
            current_values[col_index] = nom_cells[n_index]
            rowspan_remaining[col_index] = nom_rowspans[n_index]-1
            n_index += 1
        else:
            rowspan_remaining[col_index] -= 1
            #print(current_year, categories[col_index], current_values[col_index])
            mywriter.writerow([categories[col_index], current_year, current_values[col_index]])
