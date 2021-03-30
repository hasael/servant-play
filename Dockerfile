FROM arm32v7/debian:buster

RUN apt-get update && apt-get install -y gnupg2 software-properties-common
RUN echo "deb http://archive.raspbian.org/raspbian buster main contrib non-free rpi firmware" >>  /etc/apt/sources.list
RUN apt-key adv --batch --keyserver ha.pool.sks-keyservers.net  --recv-key 0x9165938D90FDDD2E 
RUN echo "deb http://archive.raspberrypi.org/debian buster main ui" >>  /etc/apt/sources.list.d/raspi.list
RUN apt-key adv --batch --keyserver ha.pool.sks-keyservers.net  --recv-key 0x82B129927FA3303E

RUN apt-get update
RUN apt-get install -y wget curl tar build-essential llvm-9 libncurses5 git
 

RUN wget http://downloads.haskell.org/~ghc/8.10.1/ghc-8.10.1-armv7-deb9-linux.tar.xz
RUN tar -xf ghc-8.10.1-armv7-deb9-linux.tar.xz
WORKDIR /ghc-8.10.1
RUN ./configure CONF_CC_OPTS_STAGE2="-marm -march=armv7-a" CFLAGS="-marm -march=armv7-a"
RUN make install
WORKDIR /
#RUN git clone https://github.com/haskell/cabal
#WORKDIR /cabal/bootstrap
#RUN bootstrap.py -d linux-8.10.1.json -w /usr/local/bin/ghc-8.10.1
#RUN cabal --version
#RUN ghc --version
#RUN cabal install Cabal cabal-install
#WORKDIR /opt/servant-play

#RUN apt-get update && apt-get install -y cabal-install 
#RUN /root/.cabal/bin/cabal update
#RUN apt-get update && apt-get install -y libpq-dev

# Add just the .cabal file to capture dependencies
#COPY ./servant-play.cabal /opt/servant-play/servant-play.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
#RUN /root/.cabal/bin/cabal install --only-dependencies -j4

# Add and Install Application Code
#COPY . /opt/servant-play
#RUN /root/.cabal/bin/cabal install

#CMD ["/opt/servant-play/dist/build/servant-play-exe/servant-play-exe"]