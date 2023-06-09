FROM ubuntu:22.04

LABEL org.label-schema.license="GPL-2.0" \
      org.label-schema.vcs-url="https://github.com/rocker-org/rocker-versioned" \
      org.label-schema.vendor="Rocker Project" \
      maintainer="Carl Boettiger <cboettig@ropensci.org>"

ARG DEBIAN_FRONTEND=noninteractive
ARG R_VERSION
ARG BUILD_DATE
ARG CRAN
ARG CTAN_REPO=${CTAN_REPO:-https://www.texlive.info/tlnet-archive/2019/02/27/tlnet}
ENV CTAN_REPO=${CTAN_REPO}
ENV PATH=$PATH:/opt/TinyTeX/bin/x86_64-linux/
ENV BUILD_DATE ${BUILD_DATE:-2023-06-07}
ENV R_VERSION=${R_VERSION:-4.3.0} \
    CRAN=${CRAN:-https://cran.rstudio.com} \ 
    TERM=xterm

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		ed \
		less \
		locales \
		vim-tiny \
		wget \
		ca-certificates \
		fonts-texgyre 

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    bash-completion \
    build-essential\
    ca-certificates \
    file \
    fonts-texgyre \
    g++ \
    gfortran \
    gsfonts \
    libblas-dev \
    libbz2-1.0 \
    libcurl4 \
    libicu-dev \
    libffi-dev \ 
    libjpeg-turbo8-dev \
    libjpeg8-dev \ 
    libjpeg-dev \
##    libjpeg62-dev \
    libopenblas-dev \
    libpangocairo-1.0-0 \
    libpcre3 \
    libpng16-16 \
    libreadline8 \
    libssl-dev\
    libtiff5 \
    liblzma5 \
    libxml2-dev\
    libxslt1-dev\
    locales\
    pkg-config\
    make \
    openssl\
    unzip \
    zip \
    zlib1g \
    zlib1g-dev\
    libxml2-dev \
##    libcairo2-dev \
    libsqlite-dev \
##    libmariadbd-dev \
##    libmariadbclient-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libsasl2-dev \
  && echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
  && locale-gen en_US.utf8 \
  && /usr/sbin/update-locale LANG=en_US.UTF-8 \
  && BUILDDEPS="curl \
    default-jdk \
    libbz2-dev \
    libcairo2-dev \
    libcurl4-openssl-dev \
    libpango1.0-dev \
    libjpeg-dev \
    libicu-dev \
    libpcre3-dev \
    libpng-dev \
    libreadline-dev \
    libtiff5-dev \
    liblzma-dev \
    libx11-dev \
    libxt-dev \
    perl \
    tcl8.6-dev \
    tk8.6-dev \
    texinfo \
    texlive-extra-utils \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texlive-latex-recommended \
    x11proto-core-dev \
    xauth \
    xfonts-base \
##    xvfb \
    zlib1g-dev" \
  && apt-get install -y --no-install-recommends $BUILDDEPS \
  && cd tmp/ 

RUN apt-get update \
  && apt-get --allow-unauthenticated install -y --no-install-recommends \
    lbzip2 \
    libfftw3-dev \
    libgdal-dev \
    libgeos-dev \
    libgsl0-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libhdf4-alt-dev \
    libhdf5-dev \
    libjq-dev \
##    liblwgeom-dev \
    libpq-dev \
    libproj-dev \
    libprotobuf-dev \
    libnetcdf-dev \
    libsqlite3-dev \
    libssl-dev \
    libudunits2-dev \
    netcdf-bin \
    postgis \
    protobuf-compiler \
    sqlite3 \
    tk-dev \
    unixodbc-dev

ENV R_BASE_VERSION 4.3.0

## During the freeze, new (source) packages are in experimental and we place the binaries in our PPA
##RUN echo "deb [trusted=yes] http://deb.debian.org/debian experimental main" > /etc/apt/sources.list.d/experimental.list \
##RUN echo "deb [trusted=yes] https://eddelbuettel.github.io/ppaR400 ./" > /etc/apt/sources.list.d/edd-r4.list 
    



  ## Now install R and littler, and create a link for littler in /usr/local/bin
RUN apt-get update \
        && apt-get install -y --no-install-recommends \
        libblis3-openmp \
        libreadline8 \
        libc6 \
        libc6-dev \
        libjpeg-turbo8 \
        libjpeg-turbo8-dev \
        liblapack3 \
		littler \
        r-cran-littler \
		r-base \
		r-base-dev \
        r-base-core \
		r-recommended \
	&& ln -s /usr/lib/R/site-library/littler/examples/install.r /usr/local/bin/install.r \
	&& ln -s /usr/lib/R/site-library/littler/examples/install2.r /usr/local/bin/install2.r \
	&& ln -s /usr/lib/R/site-library/littler/examples/installBioc.r /usr/local/bin/installBioc.r \
	&& ln -s /usr/lib/R/site-library/littler/examples/installDeps.r /usr/local/bin/installDeps.r \
	&& ln -s /usr/lib/R/site-library/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
	&& ln -s /usr/lib/R/site-library/littler/examples/testInstalled.r /usr/local/bin/testInstalled.r \
	&& install.r docopt \
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
	&& rm -rf /var/lib/apt/lists/*




RUN useradd -s /bin/bash -m docker \
	&& usermod -a -G staff docker \
## Refresh apt, install minimal tools
  && apt-get update \
	&& apt-get install -y --no-install-recommends \
		ca-certificates \
		wget \
## Install key and setup R repo at CRAN
        && wget -q -O - https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc \
                | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc  \
        && echo "deb [arch=amd64 signed-by=/etc/apt/trusted.gpg.d/cran_ubuntu_key.asc] https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/" \
                > /etc/apt/sources.list.d/cran.list \
## Install key and setup r2u repo, also set 'pin preference'
        && wget -q -O - https://r2u.stat.illinois.edu/ubuntu/dirk_eddelbuettel_pubkey.asc \
                | tee -a /etc/apt/trusted.gpg.d/dirk_eddelbuettel_pubkey.asc \
        && echo "deb [arch=amd64 signed-by=/etc/apt/trusted.gpg.d/dirk_eddelbuettel_pubkey.asc] https://r2u.stat.illinois.edu/ubuntu jammy main" \
                > /etc/apt/sources.list.d/r2u.list \
        && echo "Package: *" > /etc/apt/preferences.d/99r2u \
        && echo "Pin: release o=CRAN-Apt Project" >> /etc/apt/preferences.d/99r2u \
        && echo "Pin: release l=CRAN-Apt Packages" >> /etc/apt/preferences.d/99r2u \
        && echo "Pin-Priority: 700"  >> /etc/apt/preferences.d/99r2u \
## Configure default locale, see https://github.com/rocker-org/rocker/issues/19
        && echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
	&& locale-gen en_US.utf8 \
	&& /usr/sbin/update-locale LANG=en_US.UTF-8

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        r-base \
        r-base-dev \
        r-recommended \
## Install bspm for r2u as well as remotes and docopt used in littler script
                 r-cran-bspm \
                 r-cran-docopt \
                 r-cran-littler \
                 r-cran-remotes \
## Support user-level installation of R packages
	&& chown root:staff "/usr/local/lib/R/site-library" \
	&& chmod g+ws "/usr/local/lib/R/site-library" 
