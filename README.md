# ARCH Shiny App

## Settings

### Parameter Settings

Adjust the parameters in the environment variables:

```
export DB_HOST=your_database_host
export DB_PORT=your_database_port
export DB_NAME=your_database_name
export DB_USERNAME=your_database_username
export DB_PASSWORD=your_database_password
export URL_HOME=http://arch_landing_page_url
export URL_PHECODE=https://phenomics.va.ornl.gov/phecodemap/
export UQID_MAPPING_FILE=demo_uqid_mapping.csv               # (Optional) Linkage CSV file
```

### Documentation

The documentation files can be found in the `doc/` directory.

## Build Docker Image

Before building, please navigate to the `ARCH-shiny-app` directory in your terminal. To build the Docker image, use the following command:

```
docker build -t arch:shiny .
```

## Run Docker Container

To run the Docker container, use the following command:

```
docker run \
    -p 3838:3838 \
    -v $PWD/:/srv/shiny-server/ \
    -d arch:shiny
```

You can view the app at: http://localhost:3838/

