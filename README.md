# ARCH shiny app

## config

Adjust parameters in the `config.yaml`

## Build docker image
Please use `cd` to change to the `ARCH` directory in terminal before building.

```
docker build -t arch:shiny .
```


## Run docker container

```
docker run \
    -p 8080:3838 \
    -v $PWD/:/srv/shiny-server/ \
    -d arch:shiny
```

View the app: http://localhost:8080/

