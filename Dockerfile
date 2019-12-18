FROM erlang:22-alpine

COPY . /glot-run/

RUN export DEPS="\
	git \
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
