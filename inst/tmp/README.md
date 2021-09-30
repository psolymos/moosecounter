# tmp

Temp files that are WIP.

Docker testing (i.e. do we have all the deps for a clean install?)

```
export TAG="moose"
docker build -t $TAG .

docker run -p 8080:3838 $TAG
```

