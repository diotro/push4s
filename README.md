# push4s
 
 `push4s` is a scala implementation of the [Push](https://erp12.github.io/push-redux/) language.


To run GP in Kubernetes:

```shell script
sbt package
docker build -t push .
kubectl apply -f ./k8s/redis-pod.yml
kubectl apply -f ./k8s/redis-service.yml
kubectl apply -f ./k8s/batch.yml

# To view the results
kubectl exec -it redis-master sh
```


You may want to use minikube:
```shell script
minikube start
eval $(minikube docker-env)
```

To rebuild the docker image for GCP:
```shell script
sbt package &&
docker build -t push . && \
docker tag push gcr.io/personal-webite-241414/push && \
docker push gcr.io/personal-webite-241414/push
```
