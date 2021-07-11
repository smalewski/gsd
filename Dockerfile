# Image with stack preinstalled
FROM fpco/stack-build-small:lts-16.20

# Stack comes preinstalled

WORKDIR /home/gsd

# Copy files from repo
ADD . src/
ADD examples examples/

# Compile and install GSD
RUN cd src; stack install