FROM node:10.15.2

WORKDIR /app

COPY package.json yarn.lock ./
RUN yarn install
ENV PATH node_modules/.bin:$PATH

COPY elm-package.json ./
RUN elm package install --yes

COPY RandomGif.elm ./

EXPOSE 8000
CMD ["elm", "reactor", "--address", "0.0.0.0"]
