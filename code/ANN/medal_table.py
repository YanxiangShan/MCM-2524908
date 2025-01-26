import ast
import csv

def create_medal_table(predictions_file, discs):
    # Read the predictions from the file
    with open('results.txt', 'r') as file:
        predictions = file.read()
    
    # Convert the string back to a list of tuples
    predictions_list = ast.literal_eval(predictions)

    # Initialize a dictionary to count medals for each country
    medal_table = {}
    medal_count = 0

    # Iterate through the predictions and count the medals
    for discipline, countries in predictions_list:
        for country in countries:  # Each predicted country gets a medal
            if country.split('-')[0] in medal_table:
                medal_table[country.split('-')[0]] += 1
            else:
                medal_table[country.split('-')[0]] = 1

    sorted_medal_table = sorted(medal_table.items(), key=lambda x: x[1], reverse=True)
    sorted_medal_table_dict = {}
    
    with open('medal_table.txt', 'w') as file:
        file.write("Medal Table:\n")
        for country, count in sorted_medal_table:
            medal_count+=count
            file.write(f"{country}: {round(discs*3*count/medal_count)}\n")
        for nation, medals in sorted_medal_table:
            sorted_medal_table_dict[nation] = round(medals*discs*3/medal_count)
    return sorted_medal_table_dict


def compare_results(res_this, no_new_disciplines, no_old_disciplines):
    # ------- COMPARISON ------- # 
    res_last = {}
    Q = no_new_disciplines/no_old_disciplines

    with open('summerOly_medal_counts.csv', 'r') as file:
        reader = csv.reader(file)
        for row in reader:
            year = (row[-1])
            total_medals = (row[-2])
            country = row[1]

            if (year) == '2024':
                res_last[country] = int(total_medals)

    compare = {}
    
    for nation in res_this:
        try:
            if int(res_last[nation])*Q > int(res_this[nation]):
                compare[nation] = False #they will do worse
            elif int(res_last[nation])*Q < int(res_this[nation]):
                compare[nation] = True #they will do better
            
        except KeyError:
            compare[nation] = True
    
    do_better = open('do_better.txt', 'w')
    for key, value in compare.items():
        do_better.write(f'{key}: {value}\n')



def new_medals():
    # ------- NEW MEDAL COUNTRIES ------- #
    candidates = {}
    potential_new_medalists = []
    new_medalists = []
    with open('summerOly_medal_counts.csv', 'r') as file:
        reader = csv.reader(file)
        for row in reader:
            if row[0] == 'Rank':
                continue
            country = row[1]
            if country not in candidates:
                candidates[country] = 1
            else:
                candidates[country] += 1

        for nation in candidates:
            if candidates[nation] == 0:
                potential_new_medalists.append(nation)  


        new_results = create_medal_table('discipline_predictions.txt', 48)

        for thing in potential_new_medalists:
            try:
                if new_results[thing] > 0:
                    new_medalists.append(thing)
            except KeyError:
                continue

    return new_medalists


print(f'New Medalists: {new_medals()}')
compare_results(create_medal_table('discipline_predictions.txt', 48), 48, 50)
