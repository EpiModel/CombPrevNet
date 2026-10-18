#!/usr/bin/sh

LAST_JOB=$(tail -n 1 "out/remote_jobs/last_jobs")

cmd_get() {
    get_job_name "$@"
    . "out/remote_jobs/$JOB/get_from_ssh.sh"
}

cmd_send() {
    get_job_name "$@"

    . "out/remote_jobs/$JOB/send_to_ssh.sh"
    echo "run this command to start the jobs (copied to clipboard)"
    echo "remote_jobs/$JOB/slurm/master_slurm.sh"
    echo "remote_jobs/$JOB/slurm/master_slurm.sh" | wl-copy
}

cmd_list() {
    cat "out/remote_jobs/last_jobs"
}

cmd_usage() {
    echo "use `send` or `get` to send or get the last job to/from the server"
}

get_job_name() {
    JOB=$1
    if [ -z "$JOB" ]
    then
      JOB=$LAST_JOB
    fi

    echo "Current job is: $JOB"
}

case "$1" in
    get)  shift; cmd_get "$@" ;;
    send) shift; cmd_send "$@" ;;
    list) shift; cmd_list "$@" ;;
    *)           cmd_usage ;;
esac

exit 0
