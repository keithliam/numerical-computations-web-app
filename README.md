# Numerical Computations Web Application
This software contains numerical computation functionalities such as Quadratic Spline Interpolation and Polynomial Regression. It allows for easy uploading and modification of datasets, as well as computation and visualization of data results from numerical computations.

## Usage
#### Dependencies
Packages:
*   `r-base`

R Packages:
*   `shiny`
*   `DT`

#### Running the Program

To run the program, navigate into the program’s directory using a terminal; then execute `$ make run` to start the server. Open a web browser and type the IP address provided by the program. 


#### User Interface
![User-Interface](https://github.com/keithliam/rshiny-numerical-methods-app/blob/master/docs/user-interface.png)
1.  Quadratic Spline Interpolation tab
2.  Polynomial Regression tab
3.  Simplex tab (not implemented)
4.  File browser
5.  Verbose checkbox
6.  Input box for X
7.  Button for estimating X (uses value in input box)
8.  Slider for X
9.  Slider for Degree (Polynomial Regression only)
10. Dropdown for number of entries to show in table
11. Search box for searching table entries
12. Editable table for data

## Quadratic Spline Interpolation

##### Choosing a CSV file
To choose a CSV file, click the “Browse” button on the file browser. This will only accept files with a “.csv” file extension. If successful, the data will automatically be analyzed and plotted as a graph. 

##### Editing values in the table
To edit a value in the table, simply double-click on a table cell to make it editable. Pressing “enter” on the keyboard updates the data as well as the plotted graph.

##### Estimating  values
There are two ways to estimate a value: (1) by using the numerical input box and (2) by using the numerical slider. To use the numerical box, type the number of choice then click on the “Estimate” button. On the other hand, modifying the value to be estimated by using the slider automatically performs the calculation and shows the output.

##### Verbose
To see the functions and step-by-step computations produced and made by Quadratic Spline Interpolation, make sure that the “Verbose” checkbox is ticked. Functions and step-by-step computations will be shown when a CSV file is read, the table containing the values is edited, or an estimate is made.

## Polynomial Regression

##### Choosing a CSV file
To choose a CSV file, click the “Browse” button on the file browser. This will only accept files with a “.csv” file extension. If successful, the data will automatically be analyzed and plotted as a graph. 

##### Editing values in the table
To edit a value in the table, simply double-click on a table cell to make it editable. Pressing “enter” on the keyboard updates the data as well as the plotted graph.

##### Estimating  values
There are two ways to estimate a value: (1) by using the numerical input box and (2) by using the numerical slider. To use the numerical box, type the number of choice then click on the “Estimate” button. On the other hand, modifying the value to be estimated by using the slider automatically performs the calculation and shows the output.

##### Changing the degree of the function
To change the degree of the function, simply use the numerical slider to choose a value for the degree. The function is automatically updated when an estimate is made or the “Estimate” button is pressed.

##### Verbose
To see the function and step-by-step computations produced and made by Polynomial Regression, make sure that the “Verbose” checkbox is ticked. Functions and step-by-step computations will be shown when a CSV file is read, the table containing the values is edited, or an estimate is made. 
