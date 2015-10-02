# Machine-Learning-class---Project-1-Clustering

Phung Minh Tung - CS0801

-- Target:

Use kmeans clustering to cluster given 2688 images into k cluster


-- Working:

First of all, I read all images into R which 'list.files()' function and library 'jpeg'

To extract features from these images, I tried using some methods:
	- Use Color-histogram of small-sized windows: dividing each image into small-sized windows and take the histogram of each red-green-blue channel
	- Use Color-domination: for each image, count the number of pixels that red, green, blue, red and green, red and blue, green and blue, red and green and blue is/are having greatest value
	- Use Mean-value of small-sized windows: dividing each image into small-sized windows and take the mean of each red, green, blue channel in each window
	- Use some combination of above methods, with varied weight, change images into grayscale or remain rgb

After all, I decided to use combination of all above three with weights 0.3, 0.59 and 0.11 correspondingly
For the number of clusters, to be balance, I choose k to be equals 15
Due to a large amount of computations, this part take a relatively long time, about 15 minutes in my computer to finish.

Lastly, I composed the result into the page 'TungPM_project1_Clustering.html'.
It contains a plot of How Error varies respects to The Number of Clusters, and 20 clusters themselves.