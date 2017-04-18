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
        condition: service_started
      keycloak:
        condition: service_healthy
      columbus:
        condition: service_started
      pimp:
        condition: service_started

  hellgate:
    image: dr.rbkmoney.com/rbkmoney/hellgate:bf50fbffda6c65ebf055446dfb27ab31392a4881
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
    image: dr.rbkmoney.com/rbkmoney/machinegun:e04e529f4c5682b527d12d73a13a3cf9eb296d4d
    restart: always
    command: /opt/machinegun/bin/machinegun foreground
    volumes:
      - ./test/machinegun/sys.config:/opt/machinegun/releases/0.1.0/sys.config
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 12

  magista:
    image: dr.rbkmoney.com/rbkmoney/magista:74e1bbf7dde05b8af66ce62c8c55dc035b655f99
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
    image: dr.rbkmoney.com/rbkmoney/bustermaze:52ba5a4c3327221bd082af26ca44cebb827901fb
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
    image: dr.rbkmoney.com/rbkmoney/shumway:38c389c0132887ae3dd24e169e964fba5ab31ca3
    restart: always
    entrypoint:
      - java
      - -Xmx512m
      - -jar
      - /opt/shumway/shumway.jar
      - --spring.datasource.url=jdbc:postgresql://shumway-db:5432/shumway
      - --spring.datasource.username=postgres
      - --spring.datasource.password=postgres
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 2s
      retries: 30
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
    image: dr.rbkmoney.com/rbkmoney/dominant:e6af73a005779d5714a1d3b9e310a12f69f6fb0c
    restart: always
    command: /opt/dominant/bin/dominant foreground
    depends_on:
      machinegun:
        condition: service_healthy

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

  pimp:
    image: dr.rbkmoney.com/rbkmoney/pimp:ba9807b0d6b38ec2d65078af171c52713b5257e2
    entrypoint:
      - java
    command:
      -Xmx512m
      -jar /opt/pimp/pimp.jar

  keycloak:
    image: dr.rbkmoney.com/rbkmoney/keycloak:a4c082f48695cb02e0624deb559f9ec0378abdb4
    healthcheck:
      test: curl --silent --show-error --output /dev/null localhost:8080/auth/realms/external
      interval: 10s
      timeout: 1s
      retries: 15
    environment:
        SERVICE_NAME: keycloak
        POSTGRES_PASSWORD: keycloak
        POSTGRES_USER: keycloak
        POSTGRES_DATABASE: keycloak
        POSTGRES_PORT_5432_TCP_ADDR: keycloak-db
    depends_on:
      - keycloak-db

  keycloak-db:
    image: dr.rbkmoney.com/rbkmoney/postgres:9.6
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
