import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Dropout

# Load Data
athletes_df = pd.read_csv('summerOly_athletes.csv', delimiter=',')
hosts_df = pd.read_csv('summerOly_hosts.csv', delimiter=',')
medals_df = pd.read_csv('summerOly_medal_counts.csv', delimiter=',')

# Process Data
hosts_df["Host_Country"] = hosts_df["Host"].str.split(",").str[-1].str.strip()
hosts_df = hosts_df[["Year", "Host_Country"]]  # Maybe for host advantage later (?)

medal_with_hosts_df = pd.merge(medals_df, hosts_df, on="Year", how="left")
events_per_year = athletes_df.groupby(["Year", "Event"])["NOC"].unique().reset_index()
events_per_year.rename(columns={"NOC": "NOC_part"}, inplace=True)

complete_data = pd.merge(medal_with_hosts_df, events_per_year, on="Year", how="inner")
events_encoded = pd.get_dummies(complete_data["Event"], prefix="Event")
nocs_encoded = pd.get_dummies(complete_data["NOC"], prefix="NOC")

# Feature Columns
feature_columns = pd.concat([
    complete_data[["Gold", "Silver", "Bronze", "Total"]],
    (complete_data["NOC"] == complete_data["Host_Country"]).astype(int).rename("Host_Advantage"),
    events_encoded,
    nocs_encoded
], axis=1)

# Target Labels (One-Hot Encoding)
target = pd.get_dummies(complete_data["NOC"], prefix="Winner")

# Train-Test Split
X_train, X_test, y_train, y_test = train_test_split(feature_columns, target, test_size=0.1, random_state=42)

# Convert y_train and y_test to integers (from boolean)
y_train = y_train.astype(int)
y_test = y_test.astype(int)

# Scale Data
scaler = StandardScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)

# Model Definition
model = Sequential([
    Dense(32, input_dim=X_train.shape[1], activation='relu'),
    Dropout(0.2),
    Dense(16, activation='relu'),
    Dropout(0.2),
    Dense(y_train.shape[1], activation='softmax')  # Softmax for multi-class classification
])

# Compile the Model
model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])

# Model Training
model.fit(X_train, y_train, epochs=50, batch_size=32, validation_split=0.1)

# Model Evaluation
loss, accuracy = model.evaluate(X_test, y_test)
print(f"Test Accuracy: {accuracy:.2f}")

model.save('2025_results_predictor.keras')

predictions = model.predict(X_test)

# Function to write the top countries for each discipline to a file
def write_discipline_predictions(predictions, top_n=3):
    # Get the discipline names (event names)
    event_names = complete_data["Event"].unique()

    # Prepare a list to store the predictions
    discipline_predictions = []

    # Iterate through each event and make predictions
    for idx, event_name in enumerate(event_names):
        # Get the probabilities for the current event (discipline)
        event_probs = predictions[idx]

        # Get the countries and their corresponding probabilities
        country_labels = target.columns
        top_countries = sorted(zip(country_labels, event_probs), key=lambda x: -x[1])[:top_n]

        # Format the top countries as a list of country names
        top_country_names = [label for label, _ in top_countries]

        # Add the result for the current discipline to the list
        discipline_predictions.append((event_name, top_country_names))

    # Write the predictions to a file
    with open('discipline_predictions.txt', 'w') as file:
        file.write(str(discipline_predictions))

# Call the function to write the discipline predictions to a file
write_discipline_predictions(predictions, top_n=3)

print("Predictions saved to 'discipline_predictions.txt'.")
