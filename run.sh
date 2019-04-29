if [ "$#" -eq 1 ]; then
    UUID="$1"
elif [ -z "$UUID" ]; then
    echo "Usage: $0 UUID" >&2
    exit 123
fi

APP="/code/test/mirage/lwae"
[ -z "$APP" ] && \
    APP="/code/_build/default/test/actor_param_test_dnn.exe"

IP=$(dig +short $UUID)
[ -z "$IP" ] && (echo "No IP"; exit 123)
SERVER_IP=$(dig +short "server")
[ -z "$SERVER_IP" ] && (echo "No server IP"; exit 123)
[ -z "$SERVER_PORT" ] && SERVER_PORT=5555
[ -z "$PORT" ] && PORT=$SERVER_PORT

echo "$APP" | grep -qi "mirage"
if [ "$?" = 0 ]; then
    echo "Starting MirageOS"
    APP+=" --uuid=$UUID --ip=$IP --port=$PORT \
           --server_ip=$SERVER_IP --server_port=$SERVER_PORT -l \"*:debug\""
else
    APP+=" $UUID"
fi

$APP
