

### parameter vocabulary
https://s3-ap-southeast-2.amazonaws.com/content.aodn.org.au/Vocabularies/parameter-category/aodn_aodn-parameter-category-vocabulary.rdf


### Build,

apt-get install cabal-install
cabal install hxt
cabal install hxt-curl
cabal install warp
# cabal install warp-tls

# cabal install http-client
cabal install http-client-tls



ghc parse.hs
./parse

# for postgres,
apt-get install libpq-dev
cabal install postgresql-simple


# http clietn

http-client 
    https://haskell-lang.org/library/http-client 
    - minimalistic - no ssl/https support
    - someone else says it does have tls 

http-client-tls
    - has tls. 

http    
    https://hackage.haskell.org/package/HTTP 
    - no https - "NOTE: This package only supports HTTP; it does not support HTTPS. Attempts to use HTTPS result in an error."
    - cabal install http-conduit 
    - absolutely huge


warp
    The biggest issue with warp-tls has nothing to do with performance, but that it uses an obscure TLS implementation that has received fairly little review, and almost certainly has bugs. 
    but It has gotten top marks in every audit. We are comfortable with relying on it in production. If the alternative is an OpenSSL-based implementation,

wreq - 
    supposedly simple
    uses lenses.




