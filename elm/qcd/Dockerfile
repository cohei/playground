FROM debian:11-slim as build

RUN apt update && apt install --yes curl
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm

FROM gcr.io/distroless/static-debian11

WORKDIR /app

COPY --from=build /elm /usr/local/bin/

COPY elm.json Main.elm ./
RUN ["elm", "make", "Main.elm"]

EXPOSE 8000
ENTRYPOINT ["elm"]
CMD ["reactor"]
