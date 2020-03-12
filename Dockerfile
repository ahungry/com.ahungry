# -*- mode: dockerfile -*-
FROM ahungry/sbcl:latest

RUN yes | pacman -Sy libyaml mariadb-libs libuv

WORKDIR /app

COPY . /app/com.ahungry

# In build, load the system so its dependencies are cached.
RUN su -c '/bin/sbcl --load /app/com.ahungry/docker-build.lisp' dummy

CMD ["/bin/sbcl", "--load", "/app/com.ahungry/docker-boot.lisp"]
