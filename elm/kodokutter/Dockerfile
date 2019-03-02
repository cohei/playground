FROM node:10.15.2

WORKDIR /app

COPY package.json yarn.lock ./
RUN yarn install
ENV PATH node_modules/.bin:$PATH

COPY elm.json ./
COPY src src
RUN elm make src/Main.elm --output dist/main.js

COPY dist/index.html dist/style.css dist/

EXPOSE 8000
CMD ["elm", "reactor"]
