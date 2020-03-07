# com.ahungry

The site code for my personal site, including the Auction Logger
located at http://ahungry.com

## Installation

Make sure to install SBCL (Steel Bank Common Lisp) and setup Quicklisp
(search for instructions if you don't know about it).

## Dependencies

The underlying Quicklisp libs used for sure require the following:

- libyaml
- mariadb-libs
- libuv

## Usage

### Docker

```sh
make build && make start
```

### Native

Clone the repository, then quickload and run it with:

```lisp

(ql:quickload :com.ahungry)
(com.ahungry:start :port 5000)

```

Now just visit http://localhost:5000 in your browser, and voila!

## Author

* Matthew Carter <m@ahungry.com>

## Copyright

Copyright (c) 2014-2020 Matthew Carter <m@ahungry.com>

# License

Licensed under the AGPLv3 License (If you incorporate into your web
project, you *must* share your additions via a provided source link).
