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

$(
# ToDo: fix the config the way it works the same for CI and docker-mac environments
if [ -z "$BUILD_NUMBER" ]; then
echo "
# docker-mac environment
networks:
  default:
    driver: bridge
    enable_ipv6: true
    ipam:
      config:
        - subnet: 2001::/32
";
else
echo "
# CI environment
networks:
  default:
    driver: bridge
    driver_opts:
      com.docker.network.enable_ipv6: \"true\"
      com.docker.network.bridge.enable_ip_masquerade: \"false\"
"
fi)
EOF
