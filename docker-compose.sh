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
    image: dr.rbkmoney.com/rbkmoney/hellgate:ef9864b17f88d98a2b851a7cc090c3cfc4c19d4b
    restart: always
    command: /opt/hellgate/bin/hellgate foreground
    depends_on:
      - machinegun
      - shumway

  cds:
    image: dr.rbkmoney.com/rbkmoney/cds:538659226317356bc42529299037fe6cfa651694
    restart: always
    command: /opt/cds/bin/cds foreground

  machinegun:
    image: dr.rbkmoney.com/rbkmoney/machinegun:a48f9e93dd5a709d5f14db0c9785d43039282e86
    restart: always
    command: /opt/machinegun/bin/machinegun foreground
    volumes:
      - ./test/machinegun/sys.config:/opt/machinegun/releases/0.1.0/sys.config

  magista:
    image: dr.rbkmoney.com/rbkmoney/magista:bf7c71a9e8d7c25c901894d5fe705dc0f2efbdaa
    restart: always
    command: |
      -Xmx512m
      -jar /opt/magista/magista.jar
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
    image: dr.rbkmoney.com/rbkmoney/bustermaze:c50c584f3f2fcc6edb226712b2d241e237121ead
    restart: always
    command: |
      -Xmx512m
      -jar /opt/bustermaze/bustermaze.jar
      --spring.datasource.url=jdbc:postgresql://bustermaze-db:5432/bustermaze
      --spring.datasource.username=postgres
      --spring.datasource.password=postgres
      --hg.pooling.url=http://hellgate:8022/v1/processing/eventsink
      --flyway.url=jdbc:postgresql://bustermaze-db:5432/bustermaze
      --flyway.user=postgres
      --flyway.password=postgres
      --flyway.schemas=bm
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
    image: dr.rbkmoney.com/rbkmoney/dominant:9e6db27120f970707b77aa492fb218eb1bd40055
    restart: always
    command: /opt/dominant/bin/dominant foreground
    depends_on:
      - machinegun

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
  
  kk:
    image: dr.rbkmoney.com/rbkmoney/keycloak:1a4a81d7e3ac1bff2d41f7bed57b6619dbd92a11
    container_name: keycloak
    ports:
      - "31245:8080"
    environment:
        SERVICE_NAME: keycloak
        POSTGRES_PASSWORD: keycloak
        POSTGRES_USER: keycloak
        POSTGRES_DATABASE: keycloak
        POSTGRES_PORT_5432_TCP_ADDR: kk_db
    depends_on:
      - kk_db

  kk_db:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    container_name: keycloak_postgres
    environment:
        POSTGRES_PASSWORD: keycloak
        POSTGRES_USER: keycloak
        POSTGRES_DATABASE: keycloak

networks:
  default:
    driver: bridge
    driver_opts:
      com.docker.network.enable_ipv6: "true"
      com.docker.network.bridge.enable_ip_masquerade: "false"
EOF
