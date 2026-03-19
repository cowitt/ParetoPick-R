# =============================================================================
# STAGE 1: BASE IMAGE
# =============================================================================
# Shiny Server pre-installed (rocker means LINUX-based)
# Pinned to R 4.4.2 for reproducibility - avoid 'latest' as it can break builds
FROM rocker/shiny-verse:4.4.2


# =============================================================================
# STAGE 2: SYSTEM DEPENDENCIES
# =============================================================================
# Install all required system libraries for spatial packages (sf, terra, s2),
# image processing, SSL, and browser-based rendering (chromium for webshot2)
# STAGE 2a: Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    # --- Spatial libraries ---
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libabsl-dev \
    libudunits2-dev \
    # --- Font / graphics libraries ---
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    # --- General build / network libraries ---
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libglpk-dev \
    librsvg2-dev \
    cmake \
    git \
    gnupg \
    wget \
    # --- Chrome dependencies ---
    fonts-liberation \
    libxkbcommon0 \
    libxdamage1 \
    libxfixes3 \
    libxrandr2 \
    libgbm1 \
    libasound2t64 \
    xdg-utils \
    && rm -rf /var/lib/apt/lists/*

# STAGE 2b: Install Google Chrome separately (cannot be done via apt-get)
# Install Google Chrome via official apt repository
RUN wget -q -O - https://dl.google.com/linux/linux_signing_key.pub \
        | gpg --dearmor > /usr/share/keyrings/google-chrome.gpg \
    && echo "deb [arch=amd64 signed-by=/usr/share/keyrings/google-chrome.gpg] http://dl.google.com/linux/chrome/deb/ stable main" \
        > /etc/apt/sources.list.d/google-chrome.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends google-chrome-stable \
    && rm -rf /var/lib/apt/lists/*


# =============================================================================
# STAGE 3: ENVIRONMENT VARIABLES
# =============================================================================
# Tell webshot2/chromote where to find the Chrome and to run as root
ENV CHROMOTE_CHROME=/usr/bin/google-chrome
ENV CHROMOTE_CHROME_ARGS="--no-sandbox --disable-dev-shm-usage"
# =============================================================================
# STAGE 4: WORKING DIRECTORY
# =============================================================================
# Set working directory - all relative paths in subsequent steps resolve here
WORKDIR /srv/shiny-server/


# =============================================================================
# STAGE 5: R PACKAGE INSTALLATION
# =============================================================================

# --- Packages installed outside renv ---
# These are installed before renv::restore() either because they are:
# (a) needed to bootstrap renv itself, or
# (b) have known build issues that require fresh Linux compilation

# Install renv for reproducible package management
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org/', dependencies = TRUE)"

# Install webshot2 separately - requires chromium to be present at install time
RUN R -e "install.packages('webshot2', repos='https://cloud.r-project.org', dependencies = TRUE)"

# Install fpc separately - can have compilation issues when restored via renv
RUN R -e "install.packages('fpc', repos='https://cloud.r-project.org', dependencies = TRUE)"

# Install remotes explicitly before using it - rocker may include it but this guarantees it
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org/', dependencies = TRUE)"

# Install leaflet.extras from GitHub (not on CRAN, so cannot be handled by renv)
RUN R -e "remotes::install_github('trafficonese/leaflet.extras', dependencies = TRUE, quiet = FALSE)"

# --- Packages commented out (previously used for debugging / manual installs) ---
# These are now handled by renv::restore() via renv.lock - uncomment only if restore fails
#RUN R -e "install.packages('Rcpp',            repos='https://cloud.r-project.org/', dependencies = TRUE)"
#RUN R -e "install.packages('terra',           repos='https://cloud.r-project.org/', dependencies = TRUE)"
#RUN R -e "install.packages('digest',          repos='https://cloud.r-project.org/', dependencies = TRUE)"
#RUN R -e "install.packages('corrplot',        repos='https://cloud.r-project.org/', dependencies = TRUE)"
#RUN R -e "install.packages('DT',              repos='https://cloud.r-project.org/', dependencies = TRUE)"
#RUN R -e "install.packages('sp',              repos='https://cloud.r-project.org/', dependencies = TRUE)"
#RUN R -e "install.packages('geosphere',       repos='https://cloud.r-project.org/', dependencies = TRUE)"
#RUN R -e "install.packages('remotes',         repos='https://cloud.r-project.org/', dependencies = TRUE)"
#RUN R -e "install.packages('s2',              repos='https://cloud.r-project.org/', type='source')"
#RUN R -e "install.packages('leafsync',        repos='https://cloud.r-project.org', dependencies = TRUE)"
#RUN R -e "install.packages('patchwork',       repos='https://cloud.r-project.org', dependencies = TRUE)"
#RUN R -e "install.packages('plotly',          repos='https://cloud.r-project.org', dependencies = TRUE)"
#RUN R -e "install.packages('shiny',           repos='https://cloud.r-project.org', dependencies = TRUE)"
#RUN R -e "install.packages('tidyverse',       repos='https://cloud.r-project.org', dependencies = TRUE)"
#RUN R -e "install.packages('viridis',         repos='https://cloud.r-project.org', dependencies = TRUE)"
#RUN R -e "install.packages('quanteda',        repos='https://cloud.r-project.org', dependencies = TRUE)"
#RUN R -e "install.packages('shinycssloaders', repos='https://cloud.r-project.org', dependencies = TRUE)"
#RUN R -e "install.packages('tmap',            repos='https://cloud.r-project.org', dependencies = TRUE)"


# =============================================================================
# STAGE 6: RENV RESTORE
# =============================================================================
# Copy renv lockfile and activation script into the container,
# then restore all pinned package versions from renv.lock
COPY renv.lock renv.lock
COPY renv/ renv/
RUN R -e "renv::restore(project='/srv/shiny-server', lockfile='/srv/shiny-server/renv.lock')"


# =============================================================================
# STAGE 7: COPY APPLICATION FILES
# =============================================================================
# Copy the Shiny app and all required data folders into the container
COPY app/              /srv/shiny-server/app/
#COPY ./input           ./input
#COPY ./output          ./output
#COPY ./data            ./data

# Copy custom Shiny Server config (sets run_as, port binding, and app location)
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf


# =============================================================================
# STAGE 8: DIRECTORY CREATION & PERMISSIONS
# =============================================================================
# Ensure runtime directories exist even if the local folders were empty at build time.
# COPY does not create the destination folder if the source folder is empty,
# so mkdir -p is kept here as a safety net for empty input/output/data folders.
RUN mkdir -p \
    /srv/shiny-server/data \
    /srv/shiny-server/input \
    /srv/shiny-server/output

# Set ownership and permissions in a single RUN block to minimise image layers:
# - chown: give shiny user ownership of all app, data, and renv files
#   (shiny user is required by shiny-server.conf run_as directive)
# - chmod 775: make output and data group-writable so the app can write files at runtime
RUN chown -R shiny:shiny \
        /srv/shiny-server/app \
        /srv/shiny-server/data \
        /srv/shiny-server/input \
        /srv/shiny-server/output \
        /srv/shiny-server/renv \
        /srv/shiny-server/renv.lock \
    && chmod -R 775 \
        /srv/shiny-server/output \
        /srv/shiny-server/data

# --- Optional: run as non-root user (uncomment if required by your deployment) ---
# RUN useradd -m shinyuser -u 1000
# USER shinyuser


# =============================================================================
# STAGE 9: EXPOSE PORT & STARTUP
# =============================================================================
# Expose port 3838 - must match the port in shiny-server.conf
EXPOSE 3838

# Start Shiny Server when the container launches
CMD ["/usr/bin/shiny-server"]