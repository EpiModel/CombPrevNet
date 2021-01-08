#!/usr/bin/sh

LAST_JOB=$(tail -n 1 "out/remote_jobs/last_jobs")

cmd_get() {
    . "out/remote_jobs/$LAST_JOB/get_from_ssh.sh"
}

cmd_send() {
    . "out/remote_jobs/$LAST_JOB/send_to_ssh.sh"
    echo "run this command to start the jobs (copied to clipboard)"
    echo "remote_jobs/$LAST_JOB/slurm/master_slurm.sh"
    echo "remote_jobs/$LAST_JOB/slurm/master_slurm.sh" | wl-copy
}

cmd_list() {
    cat "out/remote_jobs/last_jobs"
}

cmd_usage() {
    echo "use `send` or `get` to send or get the last job to/from the server"
}

case "$1" in
    get) shift;                 cmd_get "$@" ;;
    send) shift;                cmd_send "$@" ;;
    list) shift;                cmd_list "$@" ;;
    *)                          cmd_usage ;;
esac

exit 0
