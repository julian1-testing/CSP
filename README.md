
## Catalog Services for Portal - PoC

- standalone prototype Geonetwork replacement written in Haskell!
- implements facet search, facet counts, freetext search, pagination, online resources, usage limits, attr constraints etc to drive AODN Portal
- csw harvesting of external catalogues
- support for mcp2.0/19139, but not dependent on metadata form - subset only, simple to manage, transform, extend
- freetext search built on top of postgres 9.5 freetext support
- iso19139 records, skos vocabulary/concepts relationally modelled. localized to db backend, supports easy cloud hosting
- logos stored and served from the db.


### Use
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


# configure portal by pointing at catalog services for portal binary
vim ./Portal.groovy
> geonetwork.url = "http://10.1.1.1:3000"

```

### Video demo

[![Demo](./resources/logo.png)](./catalogue-services-for-portal.mp4 "Demo")



