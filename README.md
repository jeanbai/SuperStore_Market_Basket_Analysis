# SuperStore_Market_Basket_Analysis
```{r}
library(arules)
library(tinytex)
library(latexpdf)
library(RColorBrewer)
library(arulesViz)

```

```{r}
data = read.csv("superstore_dataset2011-2015.csv", header = TRUE,na.strings = c("NA","","#NA"))
```

Remove the columns of Row.ID, Customer.ID,Customer.Name, Postal.Code(41296 missing valuse), and Product.ID. They don't play any contribution the target values.
```{r}
drops = c("Row.ID", "Customer.ID", "Customer.Name","Postal.Code", "Product.ID")
data=data[, !(names(data) %in% drops)]
```

```{r}
length(unique(data$Order.ID))

```

There are tatal 25035 unique Order.ID. That means there might be more than 1 times of product in each transaction. We will find out the patten what kind of products that cutomers usually purchase together.So that the store can rearrange the outlays of products, either put them together to maxmize the sales of relevant products or put them far apart so that customer have chance to see other products.

```{r}
length(unique(data$Product.Name))

```

There are 3788 kinds of products names. Remove "," in the products description for creating a transaction data.

```{r}
head(data$Product.Name)
```

```{r}
data <- data.frame(lapply(data, function(x) {
                  gsub(",", " ", x)
             }))
```

```{r}
transactions = as(split(data[,"Product.Name"], data[,"Order.ID"]),"transactions")
```


```{r}
transactions

```

```{r}
summary(transactions)
```


Density is 0.000540405. Density tells us the percentage of non-zero cells in a sparse matrix. We can calculate how many items were purchased by using the density. 


```{r}
25035 *3788 *0.000540405
```


```{r}
itemFrequencyPlot(transactions, topN = 20, type="absolute",col=brewer.pal(8,"Pastel2"), main="Absolute Item Frequency Plot")
```

Absolute will plot numeric frequencies of each item independently.

```{r}
itemFrequencyPlot(transactions, topN = 20, type="relative",col=brewer.pal(8,"Pastel2"), main="Relative Item Frequency Plot")
```

Relative plot will show how many times these items have appeared as compard to others.

### Generating Rules

Using Apriori algorithm to generate association rules.

```{r}
rules = apriori(transactions, parameter = list(supp = 0.00007, conf=0.8, maxlen=10))
```


```{r}
inspect(rules[1:10])
```


Removing redundant rules
```{r}
subset = which(colSums(is.subset(rules,rules))>1)
```
```{r}
length(subset)
```

```{r}
subset.rules = rules[-subset]
```

```{r}
staples.rules = apriori(transactions, parameter = list(supp=0.00005, conf=0.8),appearance = list(default="lhs",rhs="Staples"))
```


```{r}
inspect(head(staples.rules))
```


```{r}
subRules = rules[quality(rules)$confidence>0.5]
```


```{r}
plot(subRules)
```

```{r}
plot(subRules,method = "two-key plot")
```




```{r}
top10subRules = head(subRules, n =10, by="confidence")
```


```{r}
saveAsGraph(head(subRules, n=1000, by = "lift"), file="rules.graphml")
```

```{r}
subRules2 = head(subRules, n=20, by="lift")
plot(subRules2,method="paracoord")
```


```{r}
plotly_arules(subRules)
```

```{r}
plot(top10subRules, method="graph", engine = "htmlwidget")
```



