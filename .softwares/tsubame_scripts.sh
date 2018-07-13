#!/bin/bash
$GROUP_NAME = "jh160041"

# Args: $1 - time in format HH:MM:SS
interactive() {
    qrsh -g $GROUP_NAME -l q_node=1 -l h_rt=$1 -pty yes -display $DISPLAY -v TERM /bin/bash
}
