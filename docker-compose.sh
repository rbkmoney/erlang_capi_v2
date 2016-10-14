#!/bin/bash
cat <<EOF
version: '2'
services:

  ${SERVICE_NAME}:
    image: ${BUILD_IMAGE}
    volumes:
      - .:/$PWD
      - $HOME/.cache:/home/$UNAME/.cache
    working_dir: /$PWD
    command: /sbin/init
    depends_on:
      - hellgate
      - cds
      - magista
      - starter
      - dominant
    environment:
      - SERVICE_NAME=capi

  hellgate:
    image: dr.rbkmoney.com/rbkmoney/hellgate:d4c8e330cc7b744eb7d51d1298898f1306ae25b3
    command: /opt/hellgate/bin/hellgate foreground
    depends_on:
      - machinegun
      - shumway
    environment:
      - SERVICE_NAME=hellgate

  cds:
    image: dr.rbkmoney.com/rbkmoney/cds:42c814a7d6b1caddfd3ad96e5e28b659d15af89a
    environment:
      - SERVICE_NAME=cds

  machinegun:
    image: dr.rbkmoney.com/rbkmoney/machinegun:4c29acdcdce065dbba1f3c8ee1683caea837869c
    volumes:
      - ./test/machinegun/sys.config:/opt/machinegun/releases/0.1.0/sys.config
    environment:
      - SERVICE_NAME=machinegun

  magista:
    image: dr.rbkmoney.com/rbkmoney/magista:b18a1b11388238775d9bc330b9b89bc425ca735d
    entrypoint: |
      java
      -Xmx512m
      -jar
      /opt/magista/magista-0.0.1-SNAPSHOT.jar
    command: |
      --db.jdbc.url=jdbc:postgresql://magista_psql:5432/magista
      --db.username=magista
      --db.password=magista
      --bm.pooling.url=http://bustermaze:8081/repo
    depends_on:
      - magista_psql
      - bustermaze
    environment:
      - SERVICE_NAME=magista

  magista_psql:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DATABASE=magista
      - POSTGRES_USER=magista
      - POSTGRES_PASSWORD=magista
      - POSTGRES_ROOT_PASSWORD=magista
      - SERVICE_NAME=magista_psql

  bustermaze:
    image: dr.rbkmoney.com/rbkmoney/bustermaze:dd60a565671e34ff743218e6cb52f07e5ce632ea
    command: |
        --db.jdbc.url=jdbc:postgresql://bustermaze_psql:5432/bustermaze
        --db.username=bustermaze
        --db.password=bustermaze
        --hg.pooling.url=http://hellgate:8022/v1/processing/eventsink
    depends_on:
      - hellgate
      - bustermaze_psql
    environment:
      - SERVICE_NAME=bustermaze

  bustermaze_psql:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DATABASE=bustermaze
      - POSTGRES_USER=bustermaze
      - POSTGRES_PASSWORD=bustermaze
      - POSTGRES_ROOT_PASSWORD=bustermaze
      - SERVICE_NAME=bustermaze_psql

  shumway:
    image: dr.rbkmoney.com/rbkmoney/shumway:cd00af9d70b28a7851295fca39bdeded5a3606b0
    entrypoint: |
      java
      -Xmx512m
      -jar
      /opt/shumway/shumway-0.0.1-SNAPSHOT.jar
    command: |
      --spring.datasource.url=jdbc:postgresql://shumway_psql:5432/shumway
      --spring.datasource.username=shumway
      --spring.datasource.password=shumway
    depends_on:
      - shumway_psql
    environment:
      - SERVICE_NAME=shumway

  shumway_psql:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DATABASE=shumway
      - POSTGRES_USER=shumway
      - POSTGRES_PASSWORD=shumway
      - SERVICE_NAME=shumway_psql

  dominant:
    image: dr.rbkmoney.com/rbkmoney/dominant:c24f4d85678b8e37ee13ac3bc2c1ce0aca9fe83f
    command: /opt/dominant/bin/dominant foreground
    depends_on:
      - machinegun
    environment:
      - SERVICE_NAME=dominant

  starter:
    image: ${BUILD_IMAGE}
    volumes:
      - .:/code
    environment:
      - CDS_HOST=cds
      - SCHEMA_DIR=/code/apps/cp_proto/damsel/proto
    command:
      /code/script/cds_test_init
    depends_on:
      - cds

networks:
  default:
    driver: bridge
    driver_opts:
      com.docker.network.enable_ipv6: "true"
      com.docker.network.bridge.enable_ip_masquerade: "false"
EOF
