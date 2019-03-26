head -n 12000 ../data/new_train > ../data/security.train
tail -n 1887 ../data/new_train > ../data/security.valid
../fastText/fasttext supervised -input ../data/new_train -output ../model/model_security -epoch 10000 -lr 0.1
../fastText/fasttext test ../model/model_security.bin ../data/security.valid