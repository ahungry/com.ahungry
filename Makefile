build:
	docker build -t ahungry/com.ahungry .

start:
	docker run \
	--security-opt seccomp=unconfined \
	--rm \
	-it \
	ahungry/com.ahungry
