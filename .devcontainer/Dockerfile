FROM ubuntu:24.04

RUN apt-get update && apt-get install -y build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libnuma-dev libncurses-dev wget git
RUN wget https://downloads.haskell.org/~ghcup/aarch64-linux-ghcup -O /usr/local/bin/ghcup
RUN chmod +x /usr/local/bin/ghcup
RUN ghcup install ghc 9.4.8
RUN ghcup install cabal 3.10.3.0
RUN ghcup install stack 3.1.1
RUN ghcup install hls 2.9.0.1

RUN echo export PATH="$PATH:/root/.ghcup/bin" >> /root/.bashrc
RUN ghcup set ghc 9.10.1
RUN /root/.ghcup/bin/stack config set system-ghc --global true
