#!/bin/bash

# Command to build TAGS file
find . -type f -name '*.lisp' | etags -
