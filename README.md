<p align="justify">
<center> <h1> Prediction of Fruit and Vegetable Prices</h1> </center>

<br>

## Table of Contents  
  1. [Use Case](#use-case)  
  2. [Target Group](#target-group)  
  3. [Project Approach](#project-approach)

<br>

### Use Case 

As the prices of *fruits and vegetables* depend on a plethora of determinants, they are subject to fluctuations that might exceed seasonal price differences. Moreover, influential factors range from economic quantities to geopolitical factors. For instance, during periods of restricted international travel and trade, as global supply chains get disrupted and the supply diminishes, prices tend to rise. 

In the light of this intricate problem, this application was developed to enable a thorough analysis of the prices of fruits and vegetables in Germany from 2016 until today. To achieve our goal, prices for 23 different fruits and vegetables were observed at five wholesale markets in *Berlin*, *Cologne*, *Frankfurt*, *Hamburg* and *Munich* on a weekly basis. 

The application serves two main purposes: 

  1. **Price Analysis of Time Series Data**

We provide profound data visualization techniques and descriptive statistics to enable the user to explore how the prices of different items have developed over time. However, our main goal is to *predict prices* using **Machine Learning Methods** and compare the results with **Traditional Time Series Models**. A comparison of the prediction power can be conducted by differentiating between items, wholesale markets and models. Hence, the user can discover, which items can be predicted successfully and which fruits or vegetables seem to be rather difficult to predict. As we calculate error metrics for all approaches, the model performance can be examined for each item in the five markets. 
  
  
  2. **Examination of Nutritional Information and Ecological Footprints**

As the user might not merely be interested in the price development of the different items, the application provides detailed information about the nutritional values and ecological footprints of the fruits and vegetables. Similar to the price analysis, the user is enabled to gain knowledge about the concentration of nutrients in all items and can compare them by choosing nutrients as desired. Additionally, the ecological impact of growing the fruits and vegetables can be examined. We gathered data on both, carbon and water footprints. Based on the obtained knowledge, the user can assemble an individual food basket with all the products that satisfy the user's personal preferences.

Furthermore, the user can obtain valuable insights by using this application. By browsing through the data, the user can learn about the origin of fruits and vegetables, study the development of various determinants of the price and even gain knowledge about descriptive statistics by accessing information buttons or reading the basic information about all the algorithms implemented for the price prediction. 

Therefore, we think this application does not only add value in terms of **academic research**, but can also be seen as a simple tool for **general education**. 


<br>

### Target Group 

Since the application provides a wide range of opportunities, users from various backgrounds might find it useful to educate themselves about different topics within one environment. 

Firstly, the application is very suitable for people who are conscious about their own **health** or the **environmental impact** resulting from growing certain fruits and vegetables. For this target group, the nutrition and footprint analysis might be a convenient tool to obtain the desired information without any prerequisites regarding the knowledge of statistics or data science. 

Secondly, as we implement profound data analysis methods and machine learning algorithms, even students or people interested in **statistics and data science** can profit from our product. Beginners in the area of machine learning can get a first idea how the various algorithms work by reading through the short theory guides.

Lastly, both types of users, **private and commercial users**, can substantially benefit from this application and its price analysis and prediction features. Even though mainly larger companies purchase goods from wholesale markets, also private users can expect similar developments for retail prices in their local markets. As both prices are strongly correlated, both groups can obtain valid and reliable information.


<br>

### Project Approach 

  1. **Data Acquisition**

We applied several methods in order to access all the different sources to obtain data. Most importantly, information on weekly prices of the wholesale markets in Germany are gathered from the **Federal Office for Agriculture and Food in Germany** in *PDF* format.

Additionally, we accessed different *APIs* to add information of potential determinants of the prices to our data foundation.  

However, even rather trivial methods have proven to be efficient. For instance, the information on the environmental impact were obtained by downloading an official excel document and reading it directly into the R environment.

A detailed guide on the data acquisition process can be found within the application. 
<br>

  2. **Data Preparation**

As illustrated above, we gathered data from plenty of different sources to extract various factors, that might influence the prices, which makes the process of data preparation specifically complex. 

We aggregated all data to obtain weekly information for all variables since prices for fruits and vegetables are published once a week. 

In essence, monthly data were collapsed, so that for all calendar weeks within a month the same value is supplied. Daily data was aggregated using the *mean grouped by calendar week* resulting in one single value per week.
<br>

  3. **Data Analysis**

After completing the necessary and paramount tasks of the data preparation process, we proceeded with the data analysis. As we outlined in the beginning, we implemented simple exploratory data analysis and rather sophisticated machine learning models. 

In the exploratory data analysis we differentiate between *predictor analysis* and *price analysis*. In the modelling part, we enable the user to *predict the prices* of next week based on the most recent information. 
  
  
  
The application can be accessed [here](http://193.196.54.118:3838/shiny_apps/food_price_prediction/). 

  </p>






















