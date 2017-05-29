
## Catalog Services for Portal

- implements faceted search, and online resources, search pagination etc, to support Portal search
- csw harvest support of external geoservers
- harvest, record state and indexes, are localized in a db backend, with no filesystem


### Usage
```
# build db
psql -h postgres.localnet -U admin -d postgres -f sql/db.sql
psql -h postgres.localnet -U harvest -d harvest -f sql/tables.sql
psql -h postgres.localnet -U harvest -d harvest -f sql/views.sql


# build binaries
# see more.md for cabal deps
./build

# load vocabs
./target/LoadScheme

# harvest external catalog ie. https://catalogue-imos.aodn.org.au/geonetwork
./target/Harvest


# start portal catalog services - xml.search.imos, logos etc.
$ ./target/Service
Listening on port 3000


# test xml.search request
curl -s 'http://localhost:3000/srv/eng/xml.search.imos?protocol=OGC%3AWMS-1.1.1-http-get-map%20or%20OGC%3AWMS-1.3.0-http-get-map%20or%20IMOS%3ANCWMS--proto&sortBy=popularity&from=10&to=20&fast=index&filters=collectionavailability' | xmllint --format - | less
...


# configure portal
vim ./Portal.groovy
> geonetwork.url = "http://10.1.1.1:3000"

```



