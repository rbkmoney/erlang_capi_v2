#!/bin/bash
cat <<EOF
version: '2'
services:

  ${SERVICE_NAME}:
    image: ${BUILD_IMAGE}
    restart: always
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
    restart: always
    command: /opt/hellgate/bin/hellgate foreground
    depends_on:
      - machinegun
      - shumway
    environment:
      - SERVICE_NAME=hellgate

  cds:
    image: dr.rbkmoney.com/rbkmoney/cds:42c814a7d6b1caddfd3ad96e5e28b659d15af89a
    restart: always
    command: /opt/cds/bin/cds foreground
    environment:
      - SERVICE_NAME=cds

  machinegun:
    image: dr.rbkmoney.com/rbkmoney/machinegun:4c29acdcdce065dbba1f3c8ee1683caea837869c
    restart: always
    volumes:
      - ./test/machinegun/sys.config:/opt/machinegun/releases/0.1.0/sys.config
    environment:
      - SERVICE_NAME=machinegun

  magista:
    image: dr.rbkmoney.com/rbkmoney/magista:b18a1b11388238775d9bc330b9b89bc425ca735d
    restart: always
    command: |
      java
      -Xmx512m
      -jar /opt/magista/magista-1.0.4.jar
      --db.jdbc.url=jdbc:postgresql://magista-db:5432/magista
      --db.username=postgres
      --db.password=postgres
      --bm.pooling.url=http://bustermaze:8022/repo
    depends_on:
      - magista-db
      - bustermaze
    environment:
      - SERVICE_NAME=magista

  magista-db:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DB=magista
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
      - SERVICE_NAME=magista-db

  bustermaze:
    image: dr.rbkmoney.com/rbkmoney/bustermaze:dd60a565671e34ff743218e6cb52f07e5ce632ea
    restart: always
    command: |
      -Xmx512m
      -jar /opt/bustermaze/bustermaze.jar
      --spring.datasource.url=jdbc:postgresql://bustermaze-db:5432/bustermaze
      --spring.datasource.username=postgres
      --spring.datasource.password=postgres
      --hg.pooling.url=http://hellgate:8022/v1/processing/eventsink
    depends_on:
      - hellgate
      - bustermaze-db
    environment:
      - SERVICE_NAME=bustermaze

  bustermaze-db:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DB=bustermaze
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
      - SERVICE_NAME=bustermaze-db

  shumway:
    image: dr.rbkmoney.com/rbkmoney/shumway:cd00af9d70b28a7851295fca39bdeded5a3606b0
    restart: always
    command: |
      -Xmx512m
      -jar /opt/shumway/shumway.jar
      --spring.datasource.url=jdbc:postgresql://shumway-db:5432/shumway
      --spring.datasource.username=postgres
      --spring.datasource.password=postgres
    depends_on:
      - shumway-db
    environment:
      - SERVICE_NAME=shumway

  shumway-db:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DB=shumway
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
      - SERVICE_NAME=shumway-db

  dominant:
    image: dr.rbkmoney.com/rbkmoney/dominant:c24f4d85678b8e37ee13ac3bc2c1ce0aca9fe83f
    restart: always
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
