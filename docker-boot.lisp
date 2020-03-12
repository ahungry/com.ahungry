;; Entrypoint for the Docker loaded system

(pushnew #P"/app" ql:*local-project-directories*)

(ql:quickload :com.ahungry)

(com.ahungry:start :server :woo)
