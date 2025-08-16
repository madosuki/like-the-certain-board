FROM debian:trixie-slim

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

RUN apt-get install -y git build-essential automake autoconf make gcc libcurl4-openssl-dev curl libglib2.0-dev postgresql-client libev-dev&& \
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

RUN yes | ros update quicklisp

# install forked version lack
RUN ros install madosuki/lack/change_block_app_for_add_control_from_user_agent

RUN ros install fukamachi/caveman && \
    ros install fukamachi/ningle && \
    ros install fukamachi/quri && \
    # ros install fukamachi/lack && \
    ros install fukamachi/clack && \
    ros install cl-dbi && \
    ros install envy & \
    ros install datafly && \
    ros install kebab && \
    ros install log4cl && \
    ros install optima && \
    ros install sxql && \
    ros install trivial-utf-8 && \
    ros install madosuki/generate-like-certain-board-strings && \
    ros install madosuki/cl-markup && \ 
    ros install madosuki/cl-crypt


COPY ./roswell/init.lisp /root/.roswell/

# EXPOSE 8080

# ENTRYPOINT ["/root/.roswell/local-projects/like-the-certain-board/run.sh"]

# ENTRYPOINT ["ros"]
# CMD ["run"]

# ENTRYPOINT ["clackup"]
# CMD ["--port 8080 /root/.roswell/local-projects/like-the-ceratin-board/app.lisp"]


