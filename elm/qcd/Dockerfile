FROM node:13.10.1

WORKDIR /app

COPY package.json yarn.lock ./
RUN yarn install

COPY elm.json Main.elm ./
RUN yarn elm make Main.elm

# for elm reactor
EXPOSE 8000

CMD ["yarn", "elm", "reactor"]
