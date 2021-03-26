# Executable image for htntranslate-hpddl. 
FROM haskell:8.8.4 as builder

RUN apt-get update --assume-yes && apt-get install --assume-yes curl
ARG SSC_URLS
RUN cd /usr/local/share/ca-certificates && for i in $(echo $SSC_URLS | sed "s/,/ /g"); do curl -O "$i"; done && update-ca-certificates

WORKDIR /app/src/
COPY . /app/src/
RUN stack install --local-bin-path=/app/bin/

FROM debian:buster-slim
COPY --from=builder /app/bin/* /usr/bin/
