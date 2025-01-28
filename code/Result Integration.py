import numpy as np
from scipy.stats import gamma

try:
    data = np.loadtxt('Test.csv', delimiter=',')
except FileNotFoundError:
    raise FileNotFoundError("The file 'data.csv' was not found.")
except ValueError:
    raise ValueError("The file contains non-numeric or invalid data.")

k, theta = 2, 2
pdf_values = np.zeros_like(data)
normalized = np.zeros_like(data)

col1_min, col1_max = np.inf, -np.inf
col2_min, col2_max = np.inf, -np.inf

for i in range(data.shape[0]):
    for j in range(data.shape[1]):
        if data[i, j] == 0:
            data[i, j] =data[i, j]+0.000001



for i in range(data.shape[0]):
    for j in range(data.shape[1]):
        if data[i, j] <= 0:
            raise ValueError(f"Negative or zero value found in data at index ({i},{j}). Gamma PDF undefined.")
        pdf_values[i, j] = gamma.pdf(data[i, j], a=k, scale=theta)
        if j % 2 == 1:
            col1_min = min(col1_min, pdf_values[i, j])
            col1_max = max(col1_max, pdf_values[i, j])
        elif j % 2 == 0:
            col2_min = min(col2_min, pdf_values[i, j])
            col2_max = max(col2_max, pdf_values[i, j])

if col1_max == col1_min or col2_max == col2_min:
    raise ValueError("Normalization failed: max equals min for a column.")


for i in range(data.shape[0]):
    for j in range(data.shape[1]):
        if j % 2 ==1:
            normalized[i, j] = (pdf_values[i, j] - col1_min) / (col1_max - col1_min + 1e-5)
        elif j % 2 == 0:
            normalized[i, j] = (pdf_values[i, j] - col2_min) / (col2_max - col2_min + 1e-5)
print("the data after positive transformation is:",normalized)
X = normalized


if np.any(X < 0):
    raise ValueError("Negative values found in normalized data.")

n, m = X.shape
squere_X = X ** 2
sum_X = np.sqrt(np.sum(squere_X, axis=0))
stand_X = X / sum_X
P = stand_X / np.sum(stand_X, axis=0)
P[P == 0] = 1e-5

H_x = np.sum(-P * np.log(P), axis=0)
e_j = H_x / np.log(n)
d_j = 1 - e_j

if np.sum(d_j) == 0:
    raise ValueError("Sum of d_j is zero, unable to calculate weights.")

w = d_j / np.sum(d_j)

print("the weight of these two method are:",w)
data = np.loadtxt('Total_Test.csv', delimiter=',')

first_group = data[:, 0:4]  
second_group = data[:, 4:8]  

weighted_first_group = first_group * w[0]
weighted_second_group = second_group * w[1]

result = weighted_first_group + weighted_second_group

print("the final result about the medal earned of each country in 2028 Olynpic game is: ", result)
