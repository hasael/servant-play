FROM elmanhasa/haskell-base:local

#RUN cabal install Cabal cabal-install
WORKDIR /opt/servant-play

#RUN apt-get update && apt-get install -y cabal-install 
#RUN /root/.cabal/bin/cabal update
RUN apt-get update && apt-get install -y libpq-dev

# Add just the .cabal file to capture dependencies
COPY ./servant-play.cabal /opt/servant-play/servant-play.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies 

# Add and Install Application Code
COPY . /opt/servant-play
RUN cabal install

CMD ["/opt/servant-play/dist/build/servant-play-exe/servant-play-exe"]