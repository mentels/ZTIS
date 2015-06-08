## Running GUS analyzer ##

To run GUS portal analyzer and plot the graphs invoke:
```shell
./gus.py
make all
```

The first command will download data from the GUS portal, parse the
`bezrobotni_stopa_wg_powiatow_10_2014.xls` file and output the data
to the `output.txt` file. The second one will plot the graphs and save
them in the current directory.

## Running REGIOset.pl analyzer ##
install haskell and cabal: https://www.haskell.org/platform/

cd haskell
cabal sandbox init
cabal install
./.cabal-sandbox/bin/ztisCrawl

This command will use already crawled data in resources directory to generate histograms. 

