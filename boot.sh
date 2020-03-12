#!/bin/bash

# Gotta boot up nginx to pass through connections, as sbcl
# can only seem to listen to local connections and dies horribly
# when docker passes through to it.

# Fire off nginx in background
nginx &

# Launch the lisp server
/bin/sbcl --load /app/com.ahungry/docker-boot.lisp
