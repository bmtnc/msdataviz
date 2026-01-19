# Fundamentals Explorer Shiny App
FROM rocker/r-ver:4.4.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    unzip \
    # Build tools
    pkg-config \
    cmake \
    git \
    # Compression (httpuv/shiny needs this)
    zlib1g-dev \
    # Graphics/Fonts
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    # Image formats
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    # Git support
    libgit2-dev \
    # X11
    libx11-dev \
    # Documentation
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Install AWS CLI v2
RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-aarch64.zip" -o "awscliv2.zip" \
    && unzip awscliv2.zip \
    && ./aws/install \
    && rm -rf awscliv2.zip aws

# Set working directory
WORKDIR /app

# Install renv package for dependency management
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org/')"

# Copy renv configuration files
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Restore R packages from renv lockfile
RUN R -e "renv::restore()"

# Reinstall arrow with S3 support (renv installs minimal version)
ENV LIBARROW_MINIMAL=false
ENV ARROW_WITH_S3=true
RUN R -e "install.packages('arrow', repos='https://cloud.r-project.org/')"

# Copy package files
COPY DESCRIPTION NAMESPACE ./
COPY R/ ./R/
COPY man/ ./man/
COPY inst/ ./inst/

# Install the package using renv (so it goes into renv's library)
RUN R -e "renv::install('.', prompt = FALSE)"

# Create cache directory for artifacts
RUN mkdir -p /root/.cache/msdataviz

# Expose Shiny port
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "msdataviz::run_fundamentals_app(host = '0.0.0.0', port = 3838)"]
