FROM arm64v8/alpine:latest
 
RUN apk add curl wget xz git bash
RUN adduser -D el 
RUN mkdir -m 0755 /nix && chown el /nix
USER el
WORKDIR /home/el
RUN wget https://nixos.org/nix/install 
RUN /bin/bash ./install

RUN source /home/el/.nix-profile/etc/profile.d/nix.sh 
ENV PATH=/home/el/.nix-profile/bin/:$PATH
ENV NIX_PATH=/home/el/.nix-defexpr/channels
RUN nix-env --version

COPY . /home/el/servant-play
WORKDIR /home/el/servant-play

RUN nix-build release.nix
CMD ["/home/el/servant-play/result/bin/servant-play-exe"]