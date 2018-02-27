#!/bin/bash
#
# lisp-server-com.ahungry        Startup script for the Com.Ahungry Website
#
# chkconfig: - 85 15
# description: Website for Com.Ahungry Website
# processname: lisp-server-com.ahungry
#
### BEGIN INIT INFO
# Provides: lisp-server-com.ahungry
# Required-Start: $local_fs $remote_fs $network $named
# Required-Stop: $local_fs $remote_fs $network
# Short-Description: start and stop lisp-server-com.ahungry
# Description: Website for CC Tree
### END INIT INFO

# Source function library.
# . /etc/rc.d/init.d/functions

# Path to screen, sbcl, and the start command(s)
. ~/.env-rc-exports

screen=$(which screen)
sbcl=$(which sbcl)
opts="--eval '(ql:quickload :com.ahungry)' --eval '(com.ahungry:start :port 5000)'"
prog="Com.Ahungry"

start() {
    echo -n $"Starting $prog: "
    echo "$screen $sbcl $opts"
    $screen -S $prog -d -m $sbcl --eval '(ql:quickload :com.ahungry)' --eval '(com.ahungry:start :port 5000)'
    RETVAL=$?
    return $RETVAL
}

stop() {
    echo $"Stopping $prog"
    $screen -S $prog -X quit
}

status() {
    echo -n $"Checking on $prog: "
    $screen -ls $prog
}

restart() {
    stop
    sleep 0
    start
}

case "$1" in
    start)
        start
        ;;
    stop)
        stop
        ;;
    restart)
        restart
        ;;
    status)
        status
        ;;
    *)
        echo "Usage: $prog {start|stop}"
esac
