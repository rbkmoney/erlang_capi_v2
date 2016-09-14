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
    image: dr.rbkmoney.com/rbkmoney/hellgate:02e8d2b8f6091db6a3272b43d862248213dbd27a
    depends_on:
      - machinegun
  cds:
    image: dr.rbkmoney.com/rbkmoney/cds:dbbf05f7bcdb39a85ca12d290aeecea1bada89d1
  machinegun:
    image: dr.rbkmoney.com/rbkmoney/machinegun:cc5985c4b1ea385eba141995c37ebc67093a1fe7
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
