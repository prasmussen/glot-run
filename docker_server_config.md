# Prepare docker for running glot/$language images
##### Distro: Ubuntu 17.10
##### Docker: 17.12.0-ce


## Install docker-ce stable
[https://docs.docker.com/engine/installation/linux/docker-ce/ubuntu/#install-using-the-repository](https://docs.docker.com/engine/installation/linux/docker-ce/ubuntu/#install-using-the-repository)


## Configure docker

Configure docker to listen on tcp port 2375

Create file `/etc/systemd/system/docker.service.d/override.conf` with content:

```
[Service]
ExecStart=
ExecStart=/usr/bin/dockerd
```

Create file `/etc/docker/daemon.json` with content:

```
{
  "ip-forward": false,
  "iptables": false,
  "ipv6": false,
  "ip-masq": false,
  "hosts": ["fd://", "tcp://127.0.0.1:2375"]
}
```

Reload config and restart docker

```
systemctl daemon-reload
systemctl restart docker.service
```

## Install haproxy

Haproxy is used to expose docker on port 80 and also to restrict access from specific ip's.

```
apt-get install haproxy
```


## Configure haproxy

Append configuration to end of `/etc/haproxy/haproxy.cfg`

```
frontend docker-frontend
        bind 0.0.0.0:80
        acl network_allowed src 10.20.30.40
        tcp-request connection reject if !network_allowed
        default_backend docker-backend

backend docker-backend
        balance leastconn
        server docker localhost:2375
```
Restart haproxy

```
systemctl restart haproxy.service
```

Pull glot language images

```
docker pull glot/bash:latest
…
```

## Configure glot-run

```
…
# address to server running docker+haproxy
DOCKER_API_URL="http://1.2.3.4"
…
```

Test run bash code

```
curl -4 -sv -H "Authorization: Token some-token" -H 'Content-type: application/json' -X POST -d '{"files": [{"name": "main.sh", "content": "echo \"hello\"\n"}]}' localhost:8090/languages/bash/latest
```

