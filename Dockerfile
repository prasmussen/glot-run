FROM alpine
MAINTAINER javiertitan@gmail.com

COPY . /glot-run/

RUN export DEPS="\
	git \
	erlang \
	erlang-xmerl \
	erlang-tools \
	erlang-typer \
	erlang-snmp \
	erlang-syntax-tools \
	erlang-ssl \
	erlang-sasl \
	erlang-runtime-tools \
	erlang-ssh \
	erlang-stdlib \
	erlang-otp-mibs \
	erlang-reltool \
	erlang-mnesia \
	erlang-percept \
	erlang-parsetools \
	erlang-orber \
	erlang-public-key \
	erlang-odbc \
	erlang-os-mon \
	erlang-observer \
	erlang-et \
	erlang-ic \
	erlang-megaco \
	erlang-kernel \
	erlang-hipe \
	erlang-inets \
	erlang-jinterface \
	erlang-erts \
	erlang-gs \
	erlang-eunit \
	erlang-debugger \
	erlang-costime \
	erlang-costransaction \
	erlang-erl-interface \
	erlang-edoc \
	erlang-dialyzer \
	erlang-eldap \
	erlang-diameter \
	erlang-erl-docgen \
	erlang-crypto \
	erlang-cosevent \
	erlang-cosnotification \
	erlang-asn1 \
	erlang-cosfiletransfer \
	erlang-coseventdomain \
	erlang-dev \
	erlang-common-test \
	erlang-compiler \
	erlang-cosproperty\
 " &&\
 apk --update add $DEPS &&\
 rm -rf /var/cache/apk/* &&\
 git clone https://github.com/erlang/rebar3.git /rebar3 &&\
 cd /rebar3 &&\
 escript bootstrap &&\
 cd /glot-run &&\
 /rebar3/rebar3 compile &&\
 /rebar3/rebar3 release -c config/relx.config &&\
 mv /glot-run/_build/default/rel/glot /glot &&\
 cd / &&\
 rm -rf /rebar3 &&\
 rm -rf /glot-run &&\
 apk del $DEPS &&\
 apk --update add ncurses-libs &&\
 rm -rf /var/cache/apk/*

CMD ["/glot/bin/glot", "foreground"]
