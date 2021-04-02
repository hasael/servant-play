FROM arm64v8/alpine:latest
 
RUN apk --update add curl wget xz git bash shadow sudo 
RUN  adduser -D app   \
        && echo "app ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/app \
        && chmod 0440 /etc/sudoers.d/app
RUN mkdir -m 0755 /nix && chown app /nix
USER app


WORKDIR /home/app
RUN wget https://nixos.org/nix/install 
RUN /bin/bash ./install

RUN source /home/app/.nix-profile/etc/profile.d/nix.sh 
ENV PATH=/home/app/.nix-profile/bin/:$PATH
ENV NIX_PATH=/home/app/.nix-defexpr/channels

COPY . /home/app/servant-play
USER root
RUN chown -R app /home/app/servant-play
USER app
WORKDIR /home/app/servant-play

RUN nix-build release.nix
CMD ["/home/app/servant-play/result/bin/servant-play-exe"]