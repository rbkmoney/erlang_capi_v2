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
      - kk
      - columbus
      - pimp

  hellgate:
    image: dr.rbkmoney.com/rbkmoney/hellgate:44825a9eec9b0fc5456b45d40ee67f14173f4a5f
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
    image: dr.rbkmoney.com/rbkmoney/machinegun:cfba8560591fbc33ab5883d133849e81e237a6e1
    restart: always
    command: /opt/machinegun/bin/machinegun foreground
    volumes:
      - ./test/machinegun/sys.config:/opt/machinegun/releases/0.1.0/sys.config

  magista:
    image: dr.rbkmoney.com/rbkmoney/magista:2e0c7fb4d21ebc277e608a75a8c384505cfc711a
    restart: always
    entrypoint:
      - java
      - -Xmx512m
      - -jar
      - /opt/magista/magista.jar
      - --spring.datasource.url=jdbc:postgresql://magista-db:5432/magista
      - --spring.datasource.username=postgres
      - --spring.datasource.password=postgres
      - --bm.pooling.url=http://bustermaze:8022/repo
    depends_on:
      - magista-db
      - bustermaze

  magista-db:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DB=magista
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
    entrypoint:
     - /docker-entrypoint.sh
     - postgres

  bustermaze:
    image: dr.rbkmoney.com/rbkmoney/bustermaze:57c4cf3f9950b6ee46f67ffca286ebe8267bedde
    restart: always
    entrypoint:
      - java
      - -Xmx512m
      - -jar
      - /opt/bustermaze/bustermaze.jar
      - --spring.datasource.url=jdbc:postgresql://bustermaze-db:5432/bustermaze
      - --spring.datasource.username=postgres
      - --spring.datasource.password=postgres
      - --hg.pooling.url=http://hellgate:8022/v1/processing/eventsink
      - --flyway.url=jdbc:postgresql://bustermaze-db:5432/bustermaze
      - --flyway.user=postgres
      - --flyway.password=postgres
      - --flyway.schemas=bm
    depends_on:
      - hellgate
      - bustermaze-db

  bustermaze-db:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DB=bustermaze
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
    entrypoint:
     - /docker-entrypoint.sh
     - postgres

  shumway:
    image: dr.rbkmoney.com/rbkmoney/shumway:94e25fd3a3e7af4c73925fb051d999d7f38c271d
    restart: always
    entrypoint:
      - java
      - -Xmx512m
      - -jar
      - /opt/shumway/shumway.jar
      - --spring.datasource.url=jdbc:postgresql://shumway-db:5432/shumway
      - --spring.datasource.username=postgres
      - --spring.datasource.password=postgres
    depends_on:
      - shumway-db

  shumway-db:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
    environment:
      - POSTGRES_DB=shumway
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
    entrypoint:
     - /docker-entrypoint.sh
     - postgres

  dominant:
    image: dr.rbkmoney.com/rbkmoney/dominant:4550428dadf2ffd0886bb158be0753ae01191f01
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

  columbus:
    image:  dr.rbkmoney.com/rbkmoney/columbus:9abcea7f6833c91524604595507800588f81ef31
    ports:
     - '28022:8022'
    links:
     - columbus-postgres
    entrypoint:
       - java
       - -jar
       - /opt/columbus/columbus.jar
       - --spring.datasource.url=jdbc:postgresql://columbus-postgres:5432/columbus
       - --geo.db.file.path=file:/maxmind.mmdb
       - --logging.level.ROOT=warn
       - --logging.level.com.rbkmoney=warn

  columbus-postgres:
    image: dr.rbkmoney.com/rbkmoney/postgres-geodata:8b8df081f3f23c10079e9a41b13ce7ca2f39cd3c
    environment:
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: postgres
      POSTGRES_DB: columbus
    entrypoint:
     - /docker-entrypoint.sh
     - postgres

  pimp:
    image: dr.rbkmoney.com/rbkmoney/pimp:ba9807b0d6b38ec2d65078af171c52713b5257e2
    entrypoint:
      - java
    command:
      -Xmx512m
      -jar /opt/pimp/pimp.jar

  kk:
    image: dr.rbkmoney.com/rbkmoney/keycloak:1a4a81d7e3ac1bff2d41f7bed57b6619dbd92a11
    container_name: keycloak
    ports:
      - "31245:8080"
    healthcheck:
      test: curl --silent --show-error --output /dev/null localhost:8090
      interval: 10s
      timeout: 1s
      retries: 4
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
        POSTGRES_DB: keycloak
    entrypoint:
     - /docker-entrypoint.sh
     - postgres

networks:
  default:
    driver: bridge
    driver_opts:
      com.docker.network.enable_ipv6: "true"
      com.docker.network.bridge.enable_ip_masquerade: "false"
EOF
