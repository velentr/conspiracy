#!/bin/sh

# SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
#
# SPDX-License-Identifier: GPL-3.0-only

top_srcdir="$(pwd)"
GUILE_LOAD_PATH="$top_srcdir:$GUILE_LOAD_PATH"
export GUILE_LOAD_PATH

PATH="$top_srcdir/scripts:$PATH"
export PATH

GUILE_AUTO_COMPILE=0
export GUILE_AUTO_COMPILE

exec "$@"
