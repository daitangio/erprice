FROM erlang:24
RUN apt update && apt install -y redis-server
# Remve old rebar
RUN rm /usr/local/bin/rebar