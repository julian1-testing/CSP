
## Catalog Services for Portal - PoC

- implements PoC faceted search, freetext search, pagination, online resources, usage limit, attr constraints to drive AODN Portal
- csw harvesting of external catalogues
- support for mcp2.0/19139, but not dependent on metadata form - subset only, and easy to transform
- freetext search implemented with postgres 9.5 freetext support 
- records, concepts and indexes - all state localized to a db backend, no filesystem, support saas
- logos stored and served from the db.


### Usage
```
# build db
psql -h postgres.localnet -U admin -d postgres -f sql/db.sql
psql -h postgres.localnet -U harvest -d harvest -f sql/tables.sql
psql -h postgres.localnet -U harvest -d harvest -f sql/views.sql


# build binaries
# see doc/more.md for cabal deps
./clean.sh
./build.sh

# load vocabs
./target/LoadSchemes

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


[mcp2.0/19139](./src/ParseMCP20.hs) 

