# A reproducible environment for the manuscript "Automatically Identifying FAIR Data
# and Code Sharing Practices with Metacheck": R 4.5.3, RStudio Server, the R packages
# at the versions in renv.lock, Quarto 1.9.37 (with Typst) and LaTeX.
#
# Build, from the repository root:
#   docker build -t bes_validation .
# The build rebuilds the whole paper once and stops with an error if any number in
# the rebuilt manuscript differs from the published one.
#
# Open RStudio in a browser at http://localhost:8787 (user name: rstudio):
#   docker run --rm -p 8787:8787 -e PASSWORD=choose_a_password bes_validation
#
# Rerun the check without RStudio:
#   docker run --rm bes_validation Rscript docker/check_reproduction.R

FROM rocker/rstudio:4.5.3

# System libraries the R packages need on Ubuntu 24.04 (pak::pkg_sysreqs() for the
# packages in renv.lock).
RUN apt-get update && apt-get install -y --no-install-recommends \
      cmake git libcurl4-openssl-dev libfontconfig1-dev libfreetype6-dev \
      libfribidi-dev libglpk-dev libharfbuzz-dev libicu-dev libjpeg-dev libpng-dev \
      libpoppler-cpp-dev libssl-dev libtiff-dev libuv1-dev libwebp-dev libx11-dev \
      libxml2-dev make poppler-data fonts-dejavu \
    && rm -rf /var/lib/apt/lists/*

# Quarto 1.9.37, the version the manuscript was rendered with; it includes Typst.
RUN /rocker_scripts/install_quarto.sh 1.9.37

# LaTeX, for the appendix and the verification document. A fixed CTAN mirror: the
# default picks one at random, and some fail certificate verification in the build.
ENV CTAN_REPO=https://mirror.koddos.net/CTAN/systems/texlive/tlnet
RUN /rocker_scripts/install_texlive.sh
ENV PATH=/usr/local/texlive/bin/linux:$PATH

# The R packages, at the versions in renv.lock, as ready-made Linux packages from
# Posit's package server. Metacheck and oddpub come from GitHub at the commits the
# lock file records.
ENV RENV_CONFIG_REPOS_OVERRIDE=https://p3m.dev/cran/__linux__/noble/latest
COPY renv.lock /tmp/renv.lock
RUN Rscript -e 'install.packages("renv", repos = Sys.getenv("RENV_CONFIG_REPOS_OVERRIDE"))' \
 && Rscript -e 'renv::restore(lockfile = "/tmp/renv.lock", library = .Library.site[1], prompt = FALSE)'

# The project, and a copy of the published PDFs to compare a rebuild with.
COPY --chown=rstudio:rstudio . /home/rstudio/bes_validation
WORKDIR /home/rstudio/bes_validation
RUN mkdir -p /home/rstudio/published \
 && cp documentation/manuscript.pdf documentation/appendix.pdf documentation/manuscript_verification.pdf /home/rstudio/published/

# Rebuild the whole paper once. This stops the build if the manuscript does not
# reproduce, and it installs the LaTeX packages the documents need, so that the
# image afterwards works without internet.
RUN Rscript docker/check_reproduction.R \
 && chown -R rstudio:rstudio /home/rstudio/bes_validation /home/rstudio/published \
 && chmod -R u+rwX /home/rstudio/bes_validation

# RStudio starts in the project folder.
RUN mkdir -p /home/rstudio/.config/rstudio \
 && printf '{"initial_working_directory": "~/bes_validation"}\n' > /home/rstudio/.config/rstudio/rstudio-prefs.json \
 && chown -R rstudio:rstudio /home/rstudio/.config
