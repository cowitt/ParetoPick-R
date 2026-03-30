# =============================================================================
# STAGE 1: BASE IMAGE
# =============================================================================
FROM rocker/shiny-verse:4.4.2

# =============================================================================
# STAGE 2: SYSTEM DEPENDENCIES
# =============================================================================
# Install all system deps + Chrome in a single RUN to minimise layers.
# Build-time tools (wget, gnupg, git, cmake) are removed at the end of the
# same layer so they never persist in the image.
RUN apt-get update && apt-get install -y --no-install-recommends \
        libgdal-dev \
        libgeos-dev \
        libproj-dev \
        libabsl-dev \
        libudunits2-dev \
        libfontconfig1-dev \
        libharfbuzz-dev \
        libfribidi-dev \
        libfreetype6-dev \
        libpng-dev \
        libtiff5-dev \
        libjpeg-dev \
        libssl-dev \
        libcurl4-openssl-dev \
        libxml2-dev \
        libglpk-dev \
        librsvg2-dev \
        # Chrome runtime deps
        fonts-liberation \
        libxkbcommon0 \
        libxdamage1 \
        libxfixes3 \
        libxrandr2 \
        libgbm1 \
        libasound2t64 \
        xdg-utils \
        # Transient build tools – purged at end of this layer
        wget \
        gnupg \
        cmake \
        git \
    # ── Chrome ──────────────────────────────────────────────────────────────
    && wget -q -O - https://dl.google.com/linux/linux_signing_key.pub \
        | gpg --dearmor > /usr/share/keyrings/google-chrome.gpg \
    && echo "deb [arch=amd64 signed-by=/usr/share/keyrings/google-chrome.gpg] \
        http://dl.google.com/linux/chrome/deb/ stable main" \
        > /etc/apt/sources.list.d/google-chrome.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends google-chrome-stable \
    # ── Remove build tools & apt caches in the SAME layer ───────────────────
    && apt-get remove -y wget gnupg cmake git \
    && apt-get autoremove -y \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# STAGE 3: ENVIRONMENT VARIABLES & WORKING DIRECTORY
# =============================================================================
ENV CHROMOTE_CHROME=/usr/bin/google-chrome \
    CHROMOTE_CHROME_ARGS="--no-sandbox --disable-dev-shm-usage" \
    HOME=/home/shiny \
    # Tell renv to use a shared, purgeable cache location
    RENV_PATHS_CACHE=/tmp/renv-cache

WORKDIR /srv/shiny-server/

# =============================================================================
# STAGE 4: R PACKAGE INSTALLATION VIA RENV
# =============================================================================
COPY --chown=shiny:shiny renv.lock renv.lock
COPY --chown=shiny:shiny renv/     renv/

RUN R -e "install.packages(c('renv', 'webshot2', 'fpc', 'remotes'), \
            repos='https://cloud.r-project.org/', \
            dependencies = FALSE)" \
    && R -e "remotes::install_github('trafficonese/leaflet.extras', \
            dependencies = FALSE, quiet = FALSE)" \
    && R -e "renv::restore(project='/srv/shiny-server', \
            lockfile='/srv/shiny-server/renv.lock')" \
    # ── Purge every cache in the same layer ──────────────────────────────────
    && rm -rf \
        /tmp/* \
        /root/.cache \
        /home/shiny/.cache \
        "${RENV_PATHS_CACHE}" \
    && find /usr/local/lib/R -name "*.tar.gz" -delete \
    # Strip compiled object files that are no longer needed at runtime
    && find /usr/local/lib/R/library -name "*.o" -delete \
    && find /usr/local/lib/R/library -name "*.a" -delete

# =============================================================================
# STAGE 5: COPY APPLICATION FILES
# =============================================================================
RUN rm -rf /srv/shiny-server/*
COPY --chown=shiny:shiny app/              /srv/shiny-server/app/
COPY --chown=shiny:shiny data/             /srv/shiny-server/data/
COPY --chown=shiny:shiny input/            /srv/shiny-server/input/
COPY --chown=shiny:shiny output/           /srv/shiny-server/output/
COPY --chown=shiny:shiny shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod +x /usr/bin/shiny-server.sh

# =============================================================================
# STAGE 6: DIRECTORY CREATION & PERMISSIONS
# =============================================================================
RUN chown -R shiny:shiny \
        /srv/shiny-server/data \
        /srv/shiny-server/input \
        /srv/shiny-server/output \
        /home/shiny \
    && chmod -R 775 \
        /srv/shiny-server/data \
        /srv/shiny-server/input \
        /srv/shiny-server/output

# =============================================================================
# STAGE 7: EXPOSE PORT & STARTUP
# =============================================================================
USER shiny
EXPOSE 3838
CMD ["/usr/bin/shiny-server.sh"]