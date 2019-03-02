FROM node:10.15.2

WORKDIR /app

COPY package.json yarn.lock ./
RUN yarn install
ENV PATH node_modules/.bin:$PATH

COPY elm.json ./
COPY Main.elm ./
RUN elm make Main.elm

EXPOSE 8000
CMD ["elm", "reactor"]
