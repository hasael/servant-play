FROM elmanhasa/aarch64-haskell-base:cached
COPY . /home/app/servant-play
USER root
RUN chown -R app /home/app/servant-play
USER app
WORKDIR /home/app/servant-play

RUN nix-build release.nix
CMD ["/home/app/servant-play/result/bin/servant-play-exe","dev"]