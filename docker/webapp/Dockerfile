FROM frolvlad/alpine-glibc:alpine-3.12

WORKDIR /roswell

RUN mkdir -p /root/.roswell/local-projects/like-the-certain-board
# COPY ./run.sh ${WORKDIR}

# postgresql-client postgresql-dev postgresql-contrib

# mariadb-dev

RUN apk add --no-cache git automake autoconf make gcc build-base curl-dev curl glib-dev libressl-dev ncurses-dev sqlite libev-dev su-exec libpq mariadb-connector-c-dev fcgi-dev && \
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

ENV PATH /root/.roswell/bin:${PATH}

RUN ros install fukamachi/caveman && \
    ros install fukamachi/ningle && \
    ros install fukamachi/clack && \
    ros install cl-dbi && \
    ros install envy & \
    ros install datafly && \
    ros install kebab && \
    ros install log4cl && \
    ros install optima && \
    ros install sxql && \
    ros install trivial-utf-8 && \
    ros install madosuki/generate-like-certain-board-strings

COPY ./roswell/init.lisp /root/.roswell/

# EXPOSE 8080

# ENTRYPOINT ["/root/.roswell/local-projects/like-the-certain-board/run.sh"]

# ENTRYPOINT ["ros"]
# CMD ["run"]

# ENTRYPOINT ["clackup"]
# CMD ["--port 8080 /root/.roswell/local-projects/like-the-ceratin-board/app.lisp"]


