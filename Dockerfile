FROM ubuntu:22.04

RUN apt-get -y update && \
    apt-get -y install autoconf build-essential git lsof wget python3-pip sbcl curl

# Install quicklisp
RUN cd /tmp/ && \
    wget https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)'

# Install CCL
RUN echo '#!/bin/sh\n\
export CCL_DEFAULT_DIRECTORY=/usr/lib/ccl\n\
exec ${CCL_DEFAULT_DIRECTORY}/lx86cl64 "$@"\n\
' > /usr/bin/ccl && \
    chmod a+x /usr/bin/ccl && \
    mkdir -p /usr/lib/ccl && \
    cd /tmp && \
    git clone https://github.com/Clozure/ccl.git && \
    cd ccl && \
    git checkout v1.12.1 && \
    wget https://github.com/Clozure/ccl/releases/download/v1.12.1/linuxx86.tar.gz && \
    tar xzvf linuxx86.tar.gz -C . && \
    echo "(ccl:rebuild-ccl :full t)" | ./lx86cl64 --no-init --quiet --batch; \
    cp -pr /tmp/ccl/* /usr/lib/ccl && \
    rm -rf /tmp/ccl

# Install pre-release version of ASDF needed for CCL package-local nicknames
RUN mkdir /root/common-lisp
RUN curl https://gitlab.common-lisp.net/asdf/asdf/-/archive/3.3.7/asdf-3.3.7.tar.gz| tar xzC /root/common-lisp

WORKDIR /root/quicklisp/local-projects/
