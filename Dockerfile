# -*- mode: dockerfile -*-
FROM ahungry/sbcl

RUN yes | pacman -Sy libyaml

WORKDIR /app
COPY . /app/com.ahungry

# In build, load the system so its dependencies are cached.
RUN /bin/sbcl \
  --load /app/com.ahungry/docker-build.lisp

CMD ["/bin/sbcl", "--load", "/app/com.ahungry/docker-boot.lisp"]
