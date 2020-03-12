build:
	docker build -t ahungry/com.ahungry .

start:
	docker run \
	--env-file=.env \
	--security-opt seccomp=unconfined \
	--rm \
	-it \
	-p 5001:80 \
	ahungry/com.ahungry
