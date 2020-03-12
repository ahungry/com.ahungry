build:
	docker build -t ahungry/com.ahungry .

run:
	docker run \
	--env-file=.env \
	--security-opt seccomp=unconfined \
	--rm \
	-it \
	-p 5001:80 \
	ahungry/com.ahungry
