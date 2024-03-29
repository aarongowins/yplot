#  yplots: Yu's plot utilities

## DEMO ##
### barplot ###
```
x=data.frame(matrix(abs(rnorm(100)), ncol=5))
barplot(x, barIndex=c(1,3), label="*")
```
![](inst/extdata/figures/barplot.png)

### Pubmed Trend ###
```
term = c('"H7N9"', '"H5N1"', '"RSV"')
pm=getPubmedTrend(term, year=2001:2014)
plotPubmedTrend(pm)
```
![](inst/extdata/figures/pm.png)

## Authors ##

Guangchuang YU, School of Public Health, The University of Hong Kong [http://ygc.name](http://ygc.name)

## License ##

All source code is copyright, under the Artistic-2.0 License.
For more information on Artistic-2.0 License see [http://opensource.org/licenses/Artistic-2.0](http://opensource.org/licenses/Artistic-2.0)

## Installation ##

To install:
 * the latest development version:
   `install_github("GuangchuangYu/yplots")`

## Bugs/Feature requests ##

 - If you have any, [let me know](https://github.com/GuangchuangYu/yplots/issues). Thx!


