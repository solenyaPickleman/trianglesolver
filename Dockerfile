FROM haskell:8.10

COPY solvers  /solvers

WORKDIR /solvers/
RUN stack install random-shuffle 
RUN stack ghc -- triangle.hs -O2 -main-is Triangle -o triangle