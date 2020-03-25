FROM abhin4v/abhinavsarkar.net-base:latest AS builder

ARG REPO_URL
ARG CF_ZONE_ID
ARG CF_API_KEY
ARG CF_EMAIL
ARG STRAVA_KEY
ARG STRAVA_CLIENT_ID
ARG STRAVA_CLIENT_SECRET
ARG STRAVA_REFRESH_TOKEN
ARG DRONE_COMMIT_MESSAGE
ENV ENV=CI

ADD . .
RUN set -xe \
    && echo "127.0.0.1	abhinavsarkar.net" >> /etc/hosts \
    && echo "deb http://deb.debian.org/debian testing main" | tee /etc/apt/sources.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends libsass-dev=3.6\* \
    && rm -rf /var/lib/apt/lists/* \
    && stack --no-terminal install --flag hakyll:-previewServer --flag hakyll:-watchServer --flag hakyll:-checkExternal --fast -j2 \
    && cd _site \
    && git checkout master \
    && git pull origin master \
    && cd .. \
    && ~/.local/bin/site build \
    && cd _site \
    && git add --all \
    && bash ../bin/generate_pdfs.sh \
    && git status --short \
    && git config --global user.email "abhinav@abhinavsarkar.net" \
    && git config --global user.name "Drone.io" \
    && git commit -m "$DRONE_COMMIT_MESSAGE" \
    && git push "$REPO_URL" master \
    && (sh ../bin/purge_cf_cache.sh || true)

FROM abhin4v/hastatic:latest

COPY --from=builder /opt/abhinavsarkar.net/_site /opt/abhinavsarkar.net
COPY --from=builder /opt/abhinavsarkar.net/photos/thumbs /opt/abhinavsarkar.net/photos/thumbs
WORKDIR /opt/abhinavsarkar.net
CMD ["/usr/bin/hastatic"]
