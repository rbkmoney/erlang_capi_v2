#!/bin/bash
cat <<EOF
version: '2'

services:
  ${SERVICE_NAME}:
    image: ${BUILD_IMAGE}
    volumes:
      - .:/code
    working_dir: /code
    command: /sbin/init
    depends_on:
      - hellgate
      - cds
      - starter
  hellgate:
    image: dr.rbkmoney.com/rbkmoney/hellgate:02e8d2b8f6091db6a3272b43d862248213dbd27a
    depends_on:
      - machinegun
  cds:
    image: dr.rbkmoney.com/rbkmoney/cds:dbbf05f7bcdb39a85ca12d290aeecea1bada89d1
  machinegun:
    image: dr.rbkmoney.com/rbkmoney/mg_prototype:f981bdc338631fbdc991c78af8fd22f676c26fc7
  starter:
    image: dr.rbkmoney.com/rbkmoney/build:latest
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
