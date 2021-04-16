FROM debian:bullseye-slim

WORKDIR /roswell

RUN mkdir -p /root/.roswell/local-projects/like-the-certain-board
RUN mkdir -p /roswell/dat
RUN mkdir -p /roswell/html
RUN mkdir -p /roswell/settings
# COPY ./run.sh ${WORKDIR}

# postgresql-client postgresql-dev postgresql-contrib

RUN apt-get clean && apt-get update

RUN apt-get install -y tzdata
ENV TZ=Asia/Tokyo

RUN apt-get install -y git build-essential automake autoconf make gcc libcurl4-openssl-dev curl libglib2.0-dev default-libmysqlclient-dev libmariadb-dev-compat libfcgi-dev && \
    cd ${work_dir} && \
    git clone --depth=1 -b release https://github.com/roswell/roswell.git && \
    cd roswell && \
    sh bootstrap && \
    ./configure --disable-manual-install && \
    make && \
    make install && \
    cd .. && \
    rm -rf roswell && \
    ros run -q

# RUN cd /usr/lib/x86_64-gnu-linux && ln -s libmysqlclient.so.21 libmysqlclient_r.so && cd /roswell

ENV PATH /root/.roswell/bin:${PATH}

RUN ros install fukamachi/caveman && \
    ros install fukamachi/ningle && \
    ros install cl-dbi && \
    ros install envy & \
    ros install datafly && \
    ros install kebab && \
    ros install log4cl && \
    ros install optima && \
    ros install sxql && \
    ros install trivial-utf-8 && \
    ros install madosuki/generate-like-certain-board-strings && \
    ros install madosuki/clack


COPY ./roswell/init.lisp /root/.roswell/

# EXPOSE 8080

# ENTRYPOINT ["/root/.roswell/local-projects/like-the-certain-board/run.sh"]

# ENTRYPOINT ["ros"]
# CMD ["run"]

# ENTRYPOINT ["clackup"]
# CMD ["--port 8080 /root/.roswell/local-projects/like-the-ceratin-board/app.lisp"]


