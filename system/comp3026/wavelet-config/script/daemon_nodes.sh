source ~/.wavelet_rc.sh
source ./.wavelet_rc.sh
cd $WAVELET_HOME

for i in $(seq 1 $WAVELET_NODE_NUM)
do
    chmod a+x ${WAVELET_HOME}/scripts/node${i}.sh
    screen -d -m /bin/sh ${WAVELET_HOME}/scripts/node${i}.sh
done

# use killall screen to kill all