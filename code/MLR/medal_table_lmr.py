import csv

def compare_results():
    # predicted results format: {country: [bronze, silver, gold, total]}
    # paris results format: {country: [bronze, silver, gold, total]}

    #predicted medal table
    predicted_dict = {}
    with open('LRM/predicted_medal_table_rounded.csv', 'r') as file:
        predictions = csv.reader(file)
        for row in predictions:
            if row[0] != 'NOC':
                country = row[0].split('-')[0]
                bronze = int(row[1])
                silver = int(row[2])
                gold = int(row[3])
                total = int(row[4])

                predicted_dict[country] = [bronze, silver, gold, total]

    #paris medal table
    actual_dict = {}
    with open('CompData/summerOly_medal_counts.csv', 'r') as file:
        actual = csv.reader(file)

        for row in actual:
            if row[0] != 'Rank':
                country = row[1].split('-')[0]
                bronze = int(row[-3])
                silver = int(row[-4])
                gold = int(row[-5])
                total = int(row[-2])
                year = int(row[-1])

                if year == 2024:
                    actual_dict[country] = [bronze, silver, gold, total]

    # with both loaded dicts, compare results
    #[bronze, silver, gold, total]

    improved_countries = []
    score_gold = 5
    score_silver = 3
    score_bronze = 2

    for country in predicted_dict:
        medal_list_pred = predicted_dict[country]
        score_pred = medal_list_pred[0]*score_bronze + medal_list_pred[1]*score_silver + medal_list_pred[2]*score_gold

        medal_list_act = actual_dict[country]
        score_act = medal_list_act[0]*score_bronze + medal_list_act[1]*score_silver + medal_list_act[2]*score_gold

        if score_pred > score_act:
            improved_countries.append([country, medal_list_pred, score_pred])

    print(improved_countries)

    #[Peru, [bronze, silver, gold, total], score]

    with open('LRM/better_results.csv', 'w', newline='') as csvfile:
        flattened_improved_countries = [[item[0]] + item[1] + [item[2]] for item in improved_countries]
        writer = csv.writer(csvfile)
        writer.writerows(flattened_improved_countries)
    

def new_medalists():
    
    countries_total_medals = {}
    countries_no_medals = []

    with open('CompData/summerOly_medal_counts.csv', 'r') as file:
        actual = csv.reader(file)

        for row in actual:
            if row[0] != 'Rank':
                country = row[1].split('-')[0]
                total = int(row[-2])
                year = int(row[-1])

                if country not in countries_no_medals:
                    countries_total_medals[country] = total   

                else:
                    countries_total_medals[country] += total
    for nation, total_medals in countries_total_medals.items():
        if total_medals == 0:
            countries_no_medals.append(nation)


    return countries_no_medals

print(new_medalists())