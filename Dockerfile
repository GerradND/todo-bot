FROM haskell:8.10.4
WORKDIR /opt/todo-bot
RUN apt-get update -y
RUN apt install -y libpq-dev
COPY stack.yaml stack.yaml
COPY todo-bot.cabal todo-bot.cabal
COPY app app
COPY src src
RUN stack --install-ghc build
CMD ["stack", "run"]

# RUN apt-get update
# RUN apt-get install -y libpq-dev
# WORKDIR /opt/todo-bot
# RUN cabal update
# COPY todo-bot.cabal todo-bot.cabal
# RUN cabal build --only-dependencies -j4
# COPY app app
# COPY src src
# RUN cabal install -j4
# CMD ["todo-bot"]