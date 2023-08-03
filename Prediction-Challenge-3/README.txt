Let us see what matters in predicting your future earnings?  
You are predicting numerical variable and your error will be computed differently than for categorical variables - it will be MSE.
 Use any learning package from recitation (or any other). You can also use linear regression and as I mentioned, rpart - which works nicely for numerical variables.  Remember that knowing your data before just blindly trying different ML methods is always a good idea. Often some transformation of data (new attributes etc) will lead to better or much better results!
So plot, plot, plot.....and notice if your data has any regularities. Then use prediction packages smartly. May be subset data first?
If you just apply R-library package or plain linear regression you may end up with MSE =400,000(!) or worse....
Benchmarks from Lowest to Highest:
Showing up  MSE <100,000 
Decent result MSE <50,000
Benchmark result    MSE < 35,000   
Good result  MSE < 5000          
Excellent result MSE <1000    
Top result  MSE < 200      10 POINTS
KAGGLE DAYS  -from APRIL 29th 8PM till April 30th  8PM    - One submission only allowed by Kaggle.
ATTENTION: Error computed by MSE

IMPORTANT: Before you apply ML methods - tell us more about the data using plots. What variables matter in determining earnings?    Think about creating a new attribute which can be more predictive?   Like multiplying  number of connections by GPA?  Be creative.  You can significantly improve the ML results with a bit of "human intervention"

Submit R code and ppts. Plus KAGGLE submission - rules same as previous weeks
