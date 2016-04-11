#!/bin/sh

dir="$(pwd)"
cd "$(dirname "$0")"
home="$(pwd)"
cd "${dir}"

host="${Y_HOST:-emacs -Q --batch}"
load="${Y_LOAD:--l y}"
debug="${Y_DEBUG:+-f toggle-debug-on-error}"

exec ${host} -L "${home}" ${load} ${debug} "$@"
