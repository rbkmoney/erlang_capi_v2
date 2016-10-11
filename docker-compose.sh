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

  hellgate:
    image: dr.rbkmoney.com/rbkmoney/hellgate:5e900f7e9234cdddcd389a0233027d3bc7e37372
    depends_on:
      - machinegun
      - shumway

  cds:
    image: dr.rbkmoney.com/rbkmoney/cds:dbbf05f7bcdb39a85ca12d290aeecea1bada89d1

  machinegun:
    image: dr.rbkmoney.com/rbkmoney/machinegun:4c29acdcdce065dbba1f3c8ee1683caea837869c
    volumes:
      - ./test/machinegun/sys.config:/opt/machinegun/releases/0.1.0/sys.config
  magista:
    image: dr.rbkmoney.com/rbkmoney/magista:75c188d1b5da9d232625e53203790ecc580b3c55
    command: |
      --db.jdbc.url=jdbc:postgresql://magista_psql:5432/magista
      --db.username=magista
      --db.password=magista
      --bm.pooling.url=http://bustermaze:8081/repo
    depends_on:
      - magista_psql
      - bustermaze

  magista_psql:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DATABASE=magista
      - POSTGRES_USER=magista
      - POSTGRES_PASSWORD=magista
      - POSTGRES_ROOT_PASSWORD=magista

  bustermaze:
    image: dr.rbkmoney.com/rbkmoney/bustermaze:c205978e6ce9533eda06191da34883c71159ecc1
    command: |
        --db.jdbc.url=jdbc:postgresql://bustermaze_psql:5432/bustermaze
        --db.username=bustermaze
        --db.password=bustermaze
        --hg.pooling.url=http://hellgate:8022/v1/processing/eventsink
    depends_on:
      - hellgate
      - bustermaze_psql

  bustermaze_psql:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DATABASE=bustermaze
      - POSTGRES_USER=bustermaze
      - POSTGRES_PASSWORD=bustermaze
      - POSTGRES_ROOT_PASSWORD=bustermaze

  shumway:
    image: dr.rbkmoney.com/rbkmoney/shumway:b9487a2313ede02780a90895eb74d43e57b931f6
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

  shumway_psql:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DATABASE=shumway
      - POSTGRES_USER=shumway
      - POSTGRES_PASSWORD=shumway

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
