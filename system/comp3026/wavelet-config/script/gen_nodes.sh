source ~/.wavelet_rc.sh
source ./.wavelet_rc.sh
cd $WAVELET_HOME

mkdir -p ${WAVELET_HOME}/db ${WAVELET_HOME}/scripts ${WAVELET_HOME}/logs
rm ${WAVELET_HOME}/scripts/node*.sh -rf

cat > ${WAVELET_HOME}/scripts/node1.sh <<EOF
${WAVELET_HOME}/wavelet --wallet ${WAVELET_HOME}/wallet/wallet1.txt \
 --db ${WAVELET_HOME}/db/db1 \
 --port $WAVELET_PORT_FROM \
 --api.port $WAVELET_API_PORT_FROM \
 --genesis $WAVELET_GENESIS \
 --sys.transaction_fee_amount $WAVELET_FEE 
EOF

for i in $(seq 2 $WAVELET_NODE_NUM)
do
cat > scripts/node${i}.sh <<EOF
${WAVELET_HOME}/wavelet --wallet ${WAVELET_HOME}/wallet/wallet${i}.txt \
    --db ${WAVELET_HOME}/db/db${i} \
    --port $[${WAVELET_PORT_FROM}-1+$i] \
    --api.port $[${WAVELET_API_PORT_FROM}-1+$i] \
    --genesis $WAVELET_GENESIS \
    --sys.transaction_fee_amount $WAVELET_FEE \
    127.0.0.1:${WAVELET_PORT_FROM} 
EOF
done
