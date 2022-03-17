FROM rocker/tidyverse

# on build, copy application files
COPY . /app/

WORKDIR /app

# for installing additional dependencies etc.
RUN if [ -f '/app/onbuild' ]; then bash /app/onbuild; fi; 

# look for /app/init.R and if it exists, execute it
RUN if [ -f '/app/init.R' ]; then /usr/local/bin/R --no-init-file --no-save --quiet --slave -f /app/init.R; fi; 

# here app.R needs to match the name of the file which contains your app              
CMD cd /app && /usr/local/bin/R --no-save -f /app/src/app.R
