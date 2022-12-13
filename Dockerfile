FROM --platform=linux/amd64 haskell:8.10.4
WORKDIR /opt/todo-bot
RUN apt-get update -y
RUN apt install -y libpq-dev
RUN cabal update
COPY todo-bot.cabal todo-bot.cabal
RUN cabal install --dependencies-only -j4
COPY app app
COPY src src
RUN cabal install -j4
CMD ["todo-bot"]

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