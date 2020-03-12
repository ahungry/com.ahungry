build:
	docker build -t ahungry/com.ahungry .

start:
	docker run \
	--env-file=.env \
	--security-opt seccomp=unconfined \
	--rm \
	-it \
	-u $(shell id -u) \
	ahungry/com.ahungry
