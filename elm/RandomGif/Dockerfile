FROM node:12.1.0

WORKDIR /app

COPY package.json yarn.lock ./
RUN yarn install

COPY elm.json Main.elm ./
RUN yarn elm make Main.elm

# for elm reactor
EXPOSE 8000

CMD ["yarn", "elm", "reactor"]
