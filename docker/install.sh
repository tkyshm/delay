#!/bin/sh
set -xeu

USER=tkyshm
REPO=delay

# rebar3 install
curl -L -O https://s3.amazonaws.com/rebar3/rebar3 && install rebar3 -m 0755 /usr/local/bin

# install application
git clone git@github.com:$USER/$REPO.git

cd $REPO
/usr/local/bin/rebar3 release
ln -sf ~/$REPO/_build/default/rel/delay/bin ~/bin
