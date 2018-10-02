# CollarDownloadeR
A package to retrieve collar data from manufacturer's websites

This is very much a work in progress, but is being used by some collaborators.

### Companies
- [ATS](https://atstrack.com/)
- [Cell Track Tech](https://www.celltracktech.com/)

### In Progress
- [Lotek](http://www.lotek.com/)
- [Vectronics](https://www.vectronic-aerospace.com/)

## ATS example 
Using your login credentials we use web scraping to retrieve data from the ATS website.  The buttn_nm argument refers to the button that you would normally click to download data.  The button has a name that can be retrieved from the html of the website.  In this case we leave the bttn_nm NULL, which will download all the data.  This example will not run without actual credentials being passed to the function.

```R
my_data <- scrape_ats(bttn_nm = NULL, usr = "my_name", pwd = "my_secret")
```

## Cell Track Tech example
In this example the data are stored in tables that are linked within the website.  The website also presents the information in a multiple page table that must be navigated.  Leaving the last argument NULL will cause the function to navigate through all pages of the table and retrieve all the data tables that are found.  If only a data from a particular page is required then the user may pass the page number as the last argument.  A simple call would look like:

```R
my_data <- scrape_celltrack("my_name", "my_secret", NULL)
```
