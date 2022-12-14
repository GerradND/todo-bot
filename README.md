# todo-bot

## About

todo-bot is a discord bot written in haskell using [Calamity](https://github.com/simmsb/calamity) and [Persistent](https://github.com/yesodweb/persistent) to help you manage your daily activities with todo list.

## Features

1. **Create Todo**\
   Usage:

   ```
   !add <title> | <description> | <date in YYYY-MM-DD> | <time in HH:MM>
   ```

2. **View All Todo**\
   Usage:

   ```
   !all
   ```

3. **Check Todo Status**\
   Usage:

   ```
   !check <todoId>
   ```

4. **Uncheck Todo Status**\
   Usage:

   ```
   !uncheck <id>
   ```

5. **Edit Todo Title**\
   Usage:

   ```
   !edit-title <id> <title>
   ```

6. **Edit Todo Date**\
   Usage:

   ```
    !edit-date <id> <YYYY:MM:DD> <HH:mm>
   ```

7. **Edit Todo Description**\
   Usage:

   ```
     !edit-desc <id> <description>
   ```

8. **Edit All Todo Field**\
   Usage:

   ```
      !edit <id> | <title> | <description> | <YYYY:MM:DD> <HH:mm>
   ```

9. **Delete Todo**\
   Usage:

   ```
   !delete-todo <id>
   ```

10. **Command Helper**\
    Usage:

    ```
    !help
    or
    !help <command>
    ```

## Invite ToDoBot to your Discord Server

1. Go to the [discord calculator permission](https://discordapi.com/permissions.html)
2. Select any permission/s for the bot
3. Insert 1037017609037676585 to the Client ID Field
4. Click the link and invite to you server

## Cloning Repo

If you cloned the repo, here are the steps for you to execute the program

### Preparation

Please change the connStr on Discord.hs with your postgreSQL config.

### Building

Run the following command to build the project. Note that the first build will take a while, Calamity has a pretty heavy dependency footprint. Subsequent builds should be significantly faster.

```sh
$ stack build
```

### Running

```sh
$ stack exec todo-bot-exe
```

Now your bot is online, do some interactions with ToDoBot by using the commands mentioned above.

## Contributor

1. [Gerrad Natanael Daloma](https://github.com/GerradND)
2. [Rafi Muhammad](https://github.com/rafimuhammad01)
3. [Antonius Anggito](https://github.com/antoniusanggito/)
4. [Mario Serano](https://github.com/MarioSerano)
5. [Danan Maulidan](https://github.com/dananakbar)

## Deployment

We currently only support deployment through a dedicated virtual machine. The packages needed in the VM are as follows:

- docker
- docker-compose

Through that, the step by steps in order to deploy this application are as follows:

1. Build docker image in local machine with this command:

   ```
   docker build -t antoniusanggito/todo-bot
   ```

2. Push the docker image to the Dockerhub with this command:

   ```
   docker push antoniusanggito/todo-bot
   ```

3. Go to your VM and create `docker-compose.yml` file that defines the application and also the database. For example you can see our `docker-compose.yml` file.

4. Run the application with this command:
   ```
   docker-compose up -d
   ```

That's it, you have deployed your ToDoBot!
