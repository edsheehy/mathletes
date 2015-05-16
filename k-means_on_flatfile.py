 -*- coding: utf-8 -*-
"""

Very rough initial code to carry out k-means cluster analysis
on the flat file created by Marian

"""

import numpy as np
from scipy.cluster import vq
from matplotlib import pyplot as plt

# inputFileName = 'prices_17119_rows.txt'
# inputFileName = 'prices_100_rows.txt'
# inputFileName = 'thud_flatfile.csv'
inputFileName = 'marian_flatfile_anon.csv'

# Choose your value for the number of clusters
# numberOfClustersK = 5
numberOfClustersK = 8

# outputFileName = 'thud_cluster_codes_k' + str(numberOfClustersK) + '.txt'
outputFileName = 'marian_flatfile_anon_cluster_codes_k' + str(numberOfClustersK) + '.csv'

# outputCentersFileName = 'thud_output_Centers_k' + str(numberOfClustersK) + '.txt'
outputCentersFileName = 'marian_flatfile_anon_output_Centers_k' + str(numberOfClustersK) + '.csv'

# myArray = np.genfromtxt(inputFileName, skiprows=1)  
# myArray = np.genfromtxt(inputFileName, delimiter=",", skip_header=1, 
#                        usecols=(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
#                                 20,21,22,23,24,25,26,27,32,33,34,35))  
myArray = np.genfromtxt(inputFileName, delimiter=",", skip_header=1, 
                        usecols=(2,   4,5,6,7,8,9,10,11,12,13,14,15,   17,18,19,
                                  20,21,22,23,24,25,26,27,      34,35))  

whiteMyArray = vq.whiten(myArray)

# plt.hist(myArray,32,[0,256]),plt.show()
# plt.xlabel('Prices')
# plt.ylabel('Frequency')
# plt.title(r'Histogram of Frequency of Price Bins')

centers, dist = vq.kmeans(whiteMyArray, numberOfClustersK)

code, distance = vq.vq(whiteMyArray, centers)

# write the line number and price cluster code to a text file
outputFile = open(outputFileName, 'w')
# outputFile.write('Line_No,Cluster_Code\n')
i = 0
for item in code:
#    print(str(item))   # progress indicator for testing
    # outputFile.write(str(i) + ':::,' + str(item) + ':::\n')
    # outputFile.write(str(item) + '\n')
    outputFile.write(str(i +1) + ',' + str(code[i]) + ',' + str(distance[i]) + '\n')
    i += 1
outputFile.close()

# write the center code and value to a text file
outputCentersFile = open(outputCentersFileName, 'w')
outputCentersFile.write('Code,Value\n')
i= 0
for item in centers:
    # outputCentersFile.write(str(i) + ',' + str(item) + '\n')
    outputCentersFile.write(str(i) + ',')
    for x in item:
        outputCentersFile.write(str(x) + ', ')
    outputCentersFile.write('\n')
    i += 1
outputCentersFile.close()
