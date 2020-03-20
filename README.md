# push4s
 
 `push4s` is a scala implementation of the [Push](https://erp12.github.io/push-redux/) language.


To run GP in Kubernetes:

```shell script
sbt package
docker build -t push .
kubectl apply -f ./k8s/redis-pod.yml
kubectl apply -f ./k8s/redis-service.yml
kubectl apply -f ./k8s/batch.yml 
# or kubectl apply -f ./k8s/batch_local.yml to use your local image 

# To view the results
kubectl exec -it redis-master sh
```


You may want to use minikube:
```shell script
minikube start
eval $(minikube docker-env)
```

You may want to update Evvo, if you have a local version where you've done some development:
```shell script
./get_new_evvo.sh
```

To rebuild the docker image for GCP:
```shell script
sbt package &&
docker build -t push . && \
docker tag push gcr.io/personal-webite-241414/push && \
docker push gcr.io/personal-webite-241414/push
```
