# -*- mode: dockerfile -*-
FROM ahungry/sbcl

RUN yes | pacman -Sy libyaml mariadb mariadb-clients mariadb-libs libuv

RUN systemctl enable mariadb
RUN systemctl start mariadb

WORKDIR /app
COPY . /app/com.ahungry

# In build, load the system so its dependencies are cached.
RUN /bin/sbcl \
  --load /app/com.ahungry/docker-build.lisp

CMD ["/bin/sbcl", "--load", "/app/com.ahungry/docker-boot.lisp"]
