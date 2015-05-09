# [Digit Recognizer](http://www.kaggle.com/c/digit-recognizer)

Classify handwritten digits using the famous MNIST data.

- **Current Score** : [0.98729](http://www.kaggle.com/users/231200/andreshp).  Data preprocessing + Deep Learning.

- **Other Algorithms** : [0.97557](http://www.kaggle.com/users/231200/andreshp). Data Preprocessing + KNN with k = 1.

## Slides

Slides in Spanish explaining the work done on the problem.

## Code

### AverageImages.R

Compute the average image for each number class. The following picture shows the mean of the handwritten pictures for each digit.

![](https://github.com/andreshp/Kaggle/blob/master/DigitRecognizer/images/averages.png)

The next picture shows the mean of the handwritten pictures once they were preprocesed.

![](https://github.com/andreshp/Kaggle/blob/master/DigitRecognizer/images/averages_preprocessed.png)

### DataPreprocessing.R

Do the data preprocessing to the images. For more insight on this topic read the slides.

### DeepLearning.R

Applies [h2o](http://h2o.ai/) Deep Learning to the preprocessed data.

### KNN.R

Applies KNN to the preprocessed data.

### DataPreprocssingSum.R

Takes the preprocessed images and compute a vector with the row mean and column mean for each images. These vectors can be considered as a new data set on which learn the algorithms. However, a KNN gets 0.9 score, which is not enough.

### DataPreprocessingTotal.R

Computes a new data set where each instance is the image with the vectors explained before appended. Does not improve significantly the results with the simple DataPreprocessing.
