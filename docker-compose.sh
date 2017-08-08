#!/bin/bash
cat <<EOF
version: '2.1'
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
      hellgate:
        condition: service_started
      cds:
        condition: service_healthy
      magista:
        condition: service_started
      starter:
        condition: service_started
      dominant:
        condition: service_healthy
      keycloak:
        condition: service_healthy
      columbus:
        condition: service_started
      hooker:
        condition: service_healthy

  hellgate:
    image: dr.rbkmoney.com/rbkmoney/hellgate:f51c3aa5a5d2e956257a04147470fd99a3c003d0
    restart: always
    command: /opt/hellgate/bin/hellgate foreground
    depends_on:
      machinegun:
        condition: service_healthy
      shumway:
        condition: service_healthy

  cds:
    image: dr.rbkmoney.com/rbkmoney/cds:8cfd8eb58dbc1091e235fb427b53eb94d2a73b09
    restart: always
    command: /opt/cds/bin/cds foreground
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 12

  machinegun:
    image: dr.rbkmoney.com/rbkmoney/machinegun:535d1492b20e0151ba245cbbd3152efc70726c91
    restart: always
    command: /opt/machinegun/bin/machinegun foreground
    volumes:
      - ./test/machinegun/config.yaml:/opt/machinegun/etc/config.yaml
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 12

  magista:
    image: dr.rbkmoney.com/rbkmoney/magista:f1f3fe4ebd6f1d7da52cbadbe1de3a1368a318e8
    restart: always
    entrypoint:
      - java
      - -Xmx512m
      - -jar
      - /opt/magista/magista.jar
      - --spring.datasource.url=jdbc:postgresql://pg-db:5432/magista
      - --spring.datasource.username=postgres
      - --spring.datasource.password=postgres
      - --bm.pooling.url=http://bustermaze:8022/repo
    depends_on:
      - pg-db
      - bustermaze

  bustermaze:
    image: dr.rbkmoney.com/rbkmoney/bustermaze:52ba5a4c3327221bd082af26ca44cebb827901fb
    restart: always
    entrypoint:
      - java
      - -Xmx512m
      - -jar
      - /opt/bustermaze/bustermaze.jar
      - --spring.datasource.url=jdbc:postgresql://pg-db:5432/bustermaze
      - --spring.datasource.username=postgres
      - --spring.datasource.password=postgres
      - --hg.pooling.url=http://hellgate:8022/v1/processing/eventsink
      - --flyway.url=jdbc:postgresql://pg-db:5432/bustermaze
      - --flyway.user=postgres
      - --flyway.password=postgres
      - --flyway.schemas=bm
    depends_on:
      - hellgate
      - pg-db

  shumway:
    image: dr.rbkmoney.com/rbkmoney/shumway:38c389c0132887ae3dd24e169e964fba5ab31ca3
    restart: always
    entrypoint:
      - java
      - -Xmx512m
      - -jar
      - /opt/shumway/shumway.jar
      - --spring.datasource.url=jdbc:postgresql://pg-db:5432/shumway
      - --spring.datasource.username=postgres
      - --spring.datasource.password=postgres
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 2s
      retries: 30
    depends_on:
      - pg-db

  pg-db:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
      - PG_DBS=hooker keycloak magista shumway bustermaze
    entrypoint:
     - /docker-entrypoint.sh
     - postgres
    volumes:
      - ./test/pg-db/init-dbs.sh:/docker-entrypoint-initdb.d/init-dbs.sh

  dominant:
    image: dr.rbkmoney.com/rbkmoney/dominant:f6d260e235a9c4f418166221943f8b277267465f
    restart: always
    command: /opt/dominant/bin/dominant foreground
    depends_on:
      machinegun:
        condition: service_healthy
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 12

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
      cds:
        condition: service_healthy

  columbus:
    image:  dr.rbkmoney.com/rbkmoney/columbus:9abcea7f6833c91524604595507800588f81ef31
    links:
     - columbus-db
    entrypoint:
       - java
       - -jar
       - /opt/columbus/columbus.jar
       - --spring.datasource.url=jdbc:postgresql://columbus-db:5432/columbus
       - --geo.db.file.path=file:/maxmind.mmdb
       - --logging.level.ROOT=warn
       - --logging.level.com.rbkmoney=warn

  columbus-db:
    image: dr.rbkmoney.com/rbkmoney/postgres-geodata:8b8df081f3f23c10079e9a41b13ce7ca2f39cd3c
    environment:
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: postgres
      POSTGRES_DB: columbus
    entrypoint:
     - /docker-entrypoint.sh
     - postgres

  hooker:
    image: dr.rbkmoney.com/rbkmoney/hooker:6a9074bb00862d5c7960baee7baab2ab36d89f0d
    healthcheck:
      test: "curl -sS -o /dev/null http://localhost:8022/"
      interval: 5s
      timeout: 3s
      retries: 15
    entrypoint:
      - java
      - -jar
      - /opt/hooker/hooker.jar
      - --spring.datasource.url=jdbc:postgresql://pg-db:5432/hooker
      - --spring.datasource.username=postgres
      - --spring.datasource.password=postgres
      - --flyway.url=jdbc:postgresql://pg-db:5432/hooker
      - --flyway.user=postgres
      - --flyway.password=postgres
      - --flyway.schemas=hook
      - --bm.pooling.url=http://bustermaze:8022/repo
    depends_on:
      - pg-db

  keycloak:
    image: dr.rbkmoney.com/rbkmoney/keycloak:a4c082f48695cb02e0624deb559f9ec0378abdb4
    healthcheck:
      test: curl --silent --show-error --output /dev/null localhost:8080/auth/realms/external
      interval: 10s
      timeout: 1s
      retries: 15
    environment:
        SERVICE_NAME: keycloak
        POSTGRES_PASSWORD: postgres
        POSTGRES_USER: postgres
        POSTGRES_DATABASE: keycloak
        POSTGRES_PORT_5432_TCP_ADDR: pg-db
    depends_on:
      - pg-db

networks:
  default:
    driver: bridge
    driver_opts:
      com.docker.network.enable_ipv6: "true"
      com.docker.network.bridge.enable_ip_masquerade: "false"
EOF
