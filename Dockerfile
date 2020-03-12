# -*- mode: dockerfile -*-
FROM ahungry/sbcl:latest

RUN yes | pacman -Sy libyaml mariadb-libs libuv libev nginx gcc which

WORKDIR /app

COPY . /app/com.ahungry

# In build, load the system so its dependencies are cached.
RUN su -c '/bin/sbcl --load /app/com.ahungry/docker-build.lisp' dummy
RUN /bin/sbcl --load /app/com.ahungry/docker-build.lisp

COPY ./nginx.conf /etc/nginx
COPY ./boot.sh /app

RUN chmod +x /app/boot.sh

CMD /app/boot.sh
