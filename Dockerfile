FROM --platform=linux/amd64 haskell:8.10.4
WORKDIR /opt/todo-bot

RUN apt-get update
RUN apt-get install -y libpq-dev
WORKDIR /opt/todo-bot
RUN cabal v2-update
COPY todo-bot.cabal todo-bot.cabal
RUN cabal v2-build --only-dependencies -j1
COPY app app
COPY src src
COPY test test
RUN touch CHANGELOG.md
RUN touch README.md
RUN touch LICENSE
RUN cabal install -j4
CMD ["todo-bot-exe"]