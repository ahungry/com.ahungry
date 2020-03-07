build:
	docker build -t ahungry/com.ahungry .

start:
	docker run \
	--env-file=.env \
	--security-opt seccomp=unconfined \
	--rm \
	-it \
	ahungry/com.ahungry
