# Forecasting exchange rates and cryptocurrency prices

## Introduction

The repository contains a time series forecasting project based on exchange rates and cryptocurrency prices that comes from the [Stooq](https://stooq.com/)
The database contains closing prices of 5 cryptocurrencies and 5 conventional currencies from 2017-2021.
The main objective of the analysis was to verify the quality of fiduciary and digital currency exchange rate prediction using econometric methods and neural networks. Attention was focused on techniques focused on long memory. It was also verified whether time series have this effect. 

The structure of the project is as follows:
- First file: AR(F)IMA_modelling.R:
1. Preparation data.
2. Analysis of long memory.
3. ARIMA and ARFIMA forecasting.
- Second file: RNN_LSTM_GRU-MinMaxScaler.ipynb:
1. Environment and data.
2. Prepare machine learning pipeline functions - Simple RNN, LSTM, GRU.
3. Estimation models - Simple RNN, LSTM, GRU.
4. Evaluation - RMSE and MAPE.
5. Diebold-Mariano test.
6. Summary.

In the repository are:
* Folder data - contain data from Stooq with cryptocurrency prices and exchange rates.
* Folder Forecasting_result - contain selected best models and foreacsting result for each model for each currency.
* AR(F)IMA_modelling.R - first file contains data preparation, analysis of long memory and ARIMA, ARFIMA modelling.
* RNN_LSTM_GRU-MinMaxScaler.ipynb - second file with RNN, LSTM and GRU modelling.
* dm_test.py - third file in which Diebold-Mariano test function is keeped.
 
The whole project was carried out in June 2022 as research for a master's thesis entitled "Forecasting the rates of cryptocurrencies and classic currencies using long memory targeting methods".

Note:
Some deep learning models take quite a long time to train.

## Technologies

The following tools were mainly used for the AR(F)IMA modelling part:
* R 4.03
* Rstudio 1.3.1093
* Tidyverse
* xts
* forecast

Rest of required packages at the beginning of the script AR(F)IMA_modelling.R 

The following tools were mainly used for the deep learning modelling and evaluation part:
* python 3.9.11
* jupyterlab 3.3.2
* pandas 1.4.1
* numpy 1.19.1
* scikit-learn 1.0.1
* tensorflow 2.6.0


Rest of required packages in requirements.txt file 

## Setup
To run the project: 
1. Download the entire repository and unzip it or clone repository by git.
2. First part of analysis is a AR(F)IMA_modelling.R script. You can run of the file line by line to discover a AR(F)IMA modelling part.
3. For the second part (in Python) you have to install the required packages included in the requirements.txt file:
     * Launch any command-line interface (e.g. Anaconda Prompt).
     * If you have one, set up a custom virtual environment for the program in which the project will run.
     * Set the destination path to the folder with the project: "cd 'your destination path to project'".
     * Type "pip install -r requirements.txt".
4. Start Jupyter Notebook or JupyterLab and launch RNN_LSTM_GRU-MinMaxScaler.ipynb.
5. Review ran code. Attention: Code with model estimation takes quite a while to execute.

R script developed and tested in Rstudio 4.03. While Python program developed and tested in Jupyter Lab 3.3.2. 

